(ns main
  {:clj-kondo/config
   '{:lint-as {promesa.core/let clojure.core/let}}}
  (:require-macros
    [iconloader :refer [icon
                        load-icon
                        set-svg-base-url
                        wait-for-preload]])
  (:require
    [reagent.core :as r]
    [reagent.dom :as rdom]
    [promesa.core :as p]))

; TODO
; - inconsistent use of :url and :uuid for video uniqueness
;   (probably use url hash)
; - work out a good set of default relays - randomize?
; - use kind:5 to actually delete from relays
; - cache stored events and only request since last posted
; - use a different 300xx type than the example?

; immutable constant data

(def app-name "cx.mccormick.watchlater")
(def default-relays ["wss://relay.damus.io"
                     "wss://nostr.wine"
                     "wss://nos.lol"])
(def icon-url
  "https://cdn.jsdelivr.net/npm/@tabler/icons@3.31.0/icons/")
(def localstorage-key
  "watch-later-nostr-key")
(def localstorage-relays-key "watch-later-relays")
(def nostr-kind 30078)

(def playback-update-frequency 15000) ; 30 seconds

(def resubscribe-backoff [10 10 10 20 20 30 60])

; mutable state data

(set-svg-base-url icon-url)

(defonce state (r/atom {:loading? false
                        :videos []
                        :settings-open? false
                        :relays default-relays
                        :generated? nil
                        :sk nil
                        :modal-video nil
                        :player nil
                        :playback-timer nil
                        :subscription nil
                        :active-tab :unwatched
                        :last-settings-event-created-at nil
                        :pool nil
                        :connected-relays 0
                        :reconnect nil}))

;*** nostr functions ***;

(defn pubkey [sk]
  (js/NostrTools.getPublicKey sk))

(defn set-key [sk]
  (->> sk
       js/NostrTools.nip19.nsecEncode
       (js/localStorage.setItem localstorage-key))
  sk)

(defn generate-or-load-keys []
  (let [stored-key (js/localStorage.getItem localstorage-key)
        decoded-sk (try
                     (let [sk (js/NostrTools.nip19.decode stored-key)]
                       (and sk
                            (= (aget sk "type") "nsec")
                            (aget sk "data")))
                        (catch :default _e nil))]
    (if decoded-sk
      ; [sk generated?]
      [decoded-sk false]
      [(set-key (js/NostrTools.generateSecretKey)) true])))

(defn encrypt-content [sk content]
  (let [content-string
        (-> content
            (dissoc :event)
            clj->js
            js/JSON.stringify)]
    (js/NostrTools.nip04.encrypt sk (pubkey sk) content-string)))

(defn decrypt-content [sk pk encrypted-content]
  (try
    (let [decrypted (js/NostrTools.nip04.decrypt sk pk encrypted-content)]
      (js->clj (js/JSON.parse decrypted) :keywordize-keys true))
    (catch :default e
      (js/console.error "Failed to decrypt content" e)
      nil)))

(defn create-finalized-event [sk clj-content d-identifier]
  (js/console.log "create-finalized-event called with d-identifier:" d-identifier
                  "and content payload:" (clj->js clj-content))
  (let [encrypted-content (encrypt-content sk clj-content)
        event-template
        #js {:kind nostr-kind
             :created_at (js/Math.floor (/ (js/Date.now) 1000))
             :tags #js [#js ["d" (str app-name ":" d-identifier)]
                        #js ["n" app-name]]
             :content encrypted-content}]
    (js/console.log "Constructed event for"
                    d-identifier ":" event-template)
    (js/NostrTools.finalizeEvent event-template sk)))

(defn create-settings-event [sk settings]
  (js/console.log "create-settings-event called with settings:"
                  (clj->js settings))
  (let [settings-content {:settings settings}]
    (create-finalized-event sk settings-content "settings")))

(defn publish-event [event relays]
  (js/console.log "publish-event" event relays)
  (p/let [pool (:pool @state)
          published (js/Promise.allSettled (.publish pool (clj->js relays) event))]
    published))

(defn publish-settings! [*state]
  (let [sk (:sk *state)
        relays (map str (:relays *state))
        settings {:relays (when (not= (set relays) (set (map str default-relays)))
                            relays)}
        event (create-settings-event sk settings)]
    (if (:relays settings)
      (js/localStorage.setItem localstorage-relays-key (js/JSON.stringify (clj->js relays)))
      (js/localStorage.removeItem localstorage-relays-key))
    (publish-event event relays)))

(defn subscribe-to-events [*state sk relays event-callback eose-callback]
  (let [pk (pubkey sk)
        pool (:pool *state)
        sub (.subscribe
              pool
              (clj->js relays)
              (clj->js {:kinds [nostr-kind]
                        :authors [pk]
                        :#n [app-name]})
              (clj->js {:onevent
                        (fn [event]
                          (let [decrypted-content
                                (decrypt-content
                                  sk (pubkey sk)
                                  (.-content event))]
                            (when decrypted-content
                              (event-callback decrypted-content event))))
                        :oneose eose-callback}))]
    (assoc *state :subscription sub)))

(defn encrypt-key-with-pw [sk pw]
  (try
    (js/NostrTools.nip49.encrypt sk pw)
    (catch :default e
      (js/console.error "Failed to encrypt key" e)
      nil)))

(defn decrypt-key-with-pw [ncryptsec pw]
  (try
    (js/NostrTools.nip49.decrypt ncryptsec pw)
    (catch :default e
      (js/console.error "Failed to decrypt key" e)
      nil)))


(defn update-videos! [state decrypted-content event]
  (swap! state update :videos
         (fn [videos]
           (let [existing-index
                 (first (keep-indexed
                          (fn [idx v]
                            (when (= (:uuid v) (:uuid decrypted-content)) idx))
                          videos))]
             (if (:deleted decrypted-content)
               ; Remove deleted videos
               (if existing-index
                 (vec (concat (subvec videos 0 existing-index)
                              (subvec videos (inc existing-index))))
                 videos)
               ; Add or update non-deleted videos
               (if existing-index
                 (assoc-in videos [existing-index]
                           (assoc decrypted-content :event event))
                 (conj (or videos [])
                       (assoc decrypted-content :event event))))))))

(defn update-settings! [state decrypted-content event]
  (let [settings-payload (:settings decrypted-content)
        event-created-at (aget event "created_at")
        last-processed-settings-at (or (:last-settings-event-created-at @state)
                                       0)]
    (when (> event-created-at last-processed-settings-at)
      (js/console.log "Processing settings event:" event decrypted-content)
      (when (contains? settings-payload :relays)
        (swap! state assoc :relays (or (:relays settings-payload) default-relays)))
      (swap! state assoc :last-settings-event-created-at event-created-at))))

(defn event-received
  [decrypted-content event]
  (print "decrypted-content" decrypted-content)
  (let [d-tag (some #(when (= (aget % 0) "d") (aget % 1))
                    (.-tags event))]
    ; triage settings versus video
    (if (= d-tag (str app-name ":settings"))
      (update-settings! state decrypted-content event)
      (update-videos! state decrypted-content event))))


(defn auto-reconnect [*state connected-count]
  (if
    (> connected-count 0)
    (assoc *state :reconnect nil)
    (if-let [[backoff-idx last-attempt-ms] (:reconnect *state)]
      (let [delay-s (nth resubscribe-backoff backoff-idx)]
        (if (> (js/Date.now) (+ last-attempt-ms (* delay-s 1000)))
          (do
            (js/console.log "Attempting to reconnect to relays...")
            (let [sk (:sk *state)
                  relays (:relays *state)
                  next-idx (min (inc backoff-idx) (dec (count resubscribe-backoff)))]
              (-> *state
                  (subscribe-to-events
                    sk relays
                    #(event-received %1 %2)
                    #(swap! state assoc :eose? true))
                  (assoc :reconnect [next-idx (js/Date.now)]))))
          *state))
      (do
        (js/console.log "Connection to relays lost. Scheduling reconnect.")
        (assoc *state :reconnect [0 (js/Date.now)])))))

(defn check-connected-relays! [state]
  (when-let [pool (:pool @state)]
    (let [statuses (.listConnectionStatus pool)
          connected-count (->> statuses
                               js/Object.fromEntries
                               (js->clj)
                               (vals)
                               (filter true?)
                               count)]
      (swap! state
             (fn [*state]
               (-> *state
                   (assoc :connected-relays connected-count)
                   (auto-reconnect connected-count)))))))

(def nostr-decode js/NostrTools.nip19.decode)

(def nostr-encode-nsec js/NostrTools.nip19.nsecEncode)

(def nostr-encode-npub js/NostrTools.nip19.npubEncode)

;*** utility functions ***;

(def re-yt
  (js/RegExp.
    (str "(?:youtube\\.com\\/(?:[^\\/]+\\/.+\\/|"
         "(?:v|e(?:mbed)?)\\/|.*[?&]v=)"
         "|youtu\\.be\\/)([^\\\"&?\\/\\s]{11})")))

(defn get-youtube-id [url]
  (when url
    (let [regex re-yt
          matches (.match url regex)]
      (when (and matches (> (.-length matches) 1))
        (aget matches 1)))))

(defn get-thumbnail-url [youtube-id]
  (str "https://i3.ytimg.com/vi/" youtube-id "/mqdefault.jpg"))

(defn oembed-meta-url [youtube-id]
  (str "https://www.youtube.com/oembed"
       "?url=https://www.youtube.com/watch?v="
       youtube-id "&format=json"))

(defn fetch-youtube-metadata [youtube-id]
  (p/catch
    (p/let [url (oembed-meta-url youtube-id)
            response (js/fetch url)
            json (.json response)]
      (js->clj json :keywordize-keys true))
    js/console.error))

(defn hash-url [url]
  (p/let [encoder (js/TextEncoder.)
          data (.encode encoder url)
          hash-buffer (js/crypto.subtle.digest "SHA-256" data)
          hash-hex (.join (.map (js/Array.from (js/Uint8Array. hash-buffer))
                                #(-> %
                                     (.toString 16)
                                     (.padStart 2 "0")))
                          "")]
    (.substr hash-hex 0 8)))

(defn copy-to-clipboard [text]
  (let [el (.createElement js/document "textarea")]
    (set! (.-value el) text)
    (.appendChild (.-body js/document) el)
    (.select el)
    (.execCommand js/document "copy")
    (.removeChild (.-body js/document) el)))

(defn check-url-params []
  (let [url-params (js/URLSearchParams. (.-search js/window.location))
        key-param (.get url-params "key")]
    (when key-param
      (let [pin (js/prompt "Enter PIN to decrypt key:")]
        (when (and pin (not= pin ""))
          (let [decrypted (decrypt-key-with-pw key-param pin)]
            (when decrypted
              (let [pk (pubkey decrypted)
                    keys-obj #js {:sk decrypted :pk pk}]
                (js/localStorage.setItem "nostr-key"
                                         (js/JSON.stringify keys-obj))
                (js/window.location.reload)))))))))

(defn *publish-video-event! [relays sk {:keys [url uuid] :as video} & {:keys [delete]}]
  (let [video-content (if delete
                        {:uuid uuid
                         :url url
                         :deleted true}
                        video)
        event (create-finalized-event sk video-content uuid)]
    (publish-event event relays)))

(defn toggle-viewed! [state sk video]
  (swap! state assoc :loading? (:uuid video))
  (p/let [_ (*publish-video-event! (:relays @state) sk (update video :viewed not))]
    (swap! state assoc :loading? false)))

(defn event:delete-video! [state sk video]
  (when (js/confirm "Are you sure you want to delete this video?")
    (swap! state assoc :loading? (:uuid video))
    (p/do!
      (*publish-video-event! (:relays @state) sk video :delete true)
      (swap! state
             (fn [*state]
               (-> *state
                   (update :videos
                           (fn [videos]
                             (vec (remove #(= (:uuid %) (:uuid video)) videos))))
                   (assoc :loading? false)))))))

;*** player and modal functions ***;

(defn replace-video [videos video]
  (mapv
    #(if (= (:uuid %)
            (:uuid video))
       video %)
    videos))

(defn get-video [videos uuid]
  (->> videos
       (filter #(= uuid (:uuid %)))
       first))

(defn save-playback-time [sk video current-time & [force-network-write]]
  (js/console.log "save-playback-time" (:uuid video))
  ;(print video)
  (when (and video current-time (> current-time 0))
    (let [now (js/Date.now)
          last-write (get-in video [:playback :last-write])
          re-write? (or force-network-write
                        (< last-write (- now playback-update-frequency)))
          updated-video (update-in video [:playback] assoc
                                   :time current-time
                                   :last-write (if re-write? now last-write))]
      ;(print "updated-video" updated-video)
      (swap! state update-in [:videos] replace-video updated-video)
      (when re-write?
        (js/console.log "save-playback-time network save")
        (*publish-video-event! (:relays @state) sk updated-video)))))

(defn setup-playback-tracking [state sk uuid]
  (when-let [timer (:playback-timer @state)]
    (js/clearInterval timer))
  (let [timer (js/setInterval
                (fn []
                  (some->> @state
                           :player
                           .getCurrentTime
                           (save-playback-time
                             sk
                             (get-video (:videos @state) uuid))))
                1000)]
    (swap! state assoc :playback-timer timer)))

(defn stop-playback-tracking [state]
  (when-let [timer (:playback-timer @state)]
    (js/clearInterval timer)
    (swap! state assoc :playback-timer nil)))

(defn on-player-state-change [state sk uuid event]
  (let [state-code (.-data event)]
    (cond
      ; Video ended (state 0)
      (= state-code 0)
      (let [video (get-video (:videos @state) uuid)]
        (stop-playback-tracking state)
        ; Reset playback time and mark as viewed
        (toggle-viewed! state sk
                        (-> video
                            (assoc-in [:playback :time] 0)
                            (assoc :viewed false)))
        (swap! state assoc :modal-video nil :player nil))

      ; Video playing (state 1)
      (= state-code 1)
      (setup-playback-tracking state sk uuid)

      ; Video paused or other states
      :else
      (stop-playback-tracking state))))

(defn on-player-ready [video event]
  (let [player (.-target event)
        start-time (or (-> video :playback :time) 0)]
    (swap! state assoc :player player)
    (when (> start-time 0)
      (.seekTo player start-time true))))

(defn create-youtube-player [youtube-id sk video]
  (js/setTimeout
    (fn []
      (when js/window.YT
        (js/window.YT.Player.
          "youtube-player"
          #js {:height "100%"
               :width "100%"
               :videoId youtube-id
               :playerVars #js {:autoplay 1
                                :controls 1
                                :rel 0
                                :modestbranding 1}
               :events #js {:onReady #(on-player-ready video %)
                            :onStateChange #(on-player-state-change
                                              state sk (:uuid video) %)}})))
    100))

(defn event:open-video-modal [sk video]
  (js/console.log "Opening video modal" (:url video))
  (let [youtube-id (get-youtube-id (:url video))]
    (js/console.log "YouTube ID:" youtube-id)
    (swap! state assoc :modal-video video)
    (create-youtube-player youtube-id sk video)))

(defn event:close-video-modal []
  ; Save current playback time before closing
  (when-let [player (:player @state)]
    (save-playback-time (:sk @state)
                        (get-video (:videos @state)
                                   (-> @state :modal-video :uuid)) (.getCurrentTime player) :force))
  (stop-playback-tracking state)
  (swap! state assoc :modal-video nil :player nil))

;*** components and events ***;

(defn component:video-modal []
  (when (:modal-video @state)
    [:div.modal-overlay
     {:on-click #(when (= (.-target %) (.-currentTarget %))
                   (event:close-video-modal))}
     [:div.modal-content
      [:button.modal-close
       {:on-click event:close-video-modal}
       [icon (load-icon "outline/x.svg")]]
      [:div#youtube-player]]]))

(defn component:loading-spinner [attrs]
  [:div.loading attrs [:div]])

(defn component:video-item [sk {:keys [url viewed uuid event
                                       metadata playback]}]
  ;(js/console.log "video-item render" url viewed)
  (let [youtube-id (get-youtube-id url)
        thumbnail-url (get-thumbnail-url youtube-id)
        title (if (and metadata (:title metadata))
                (:title metadata)
                "YouTube Video")
        video-data {:url url :viewed viewed :uuid uuid :event event
                    :metadata metadata
                    :playback playback}]
    [:div.video-item {:class (when viewed "viewed")}
     [:div.thumbnail-container
      [:div.clickable-area
       {:on-click #(event:open-video-modal sk video-data)}
       [:img.thumbnail {:src thumbnail-url :alt "Video thumbnail"}]]
      [:row-group
       [:div.video-title
        {:on-click #(event:open-video-modal sk video-data)}
        title]
       [:div.video-controls
        [:button.icon-button
         {:on-click #(toggle-viewed! state sk video-data)
          :alt (if viewed "Viewed" "Mark as viewed")}
         (if (= (:loading? @state) uuid)
           [component:loading-spinner {:data-size "small"}]
           [icon
            (if viewed
              (load-icon "filled/eye.svg")
              (load-icon "outline/eye.svg"))])]
        [:button.icon-button
         {:on-click #(event:delete-video! state sk video-data)
          :alt "Delete video"}
         [icon (load-icon "outline/trash.svg")]]]]]]))

(defn event:pasted-url [state input-value ev]
  (let [pasted-text (.. ev -clipboardData (getData "text"))]
    (when (get-youtube-id pasted-text)
      (reset! input-value pasted-text)
      (js/setTimeout
        (fn []
          (let [sk (:sk @state)
                youtube-id (get-youtube-id pasted-text)
                new-uuid (str (random-uuid))]
            (swap! state assoc :loading? true)
            (p/let [metadata (fetch-youtube-metadata youtube-id)
                    video-data {:uuid new-uuid
                                :url pasted-text
                                :viewed false
                                :metadata metadata
                                :playback
                                {:time 0
                                 :last-write 0}}
                    published (*publish-video-event! (:relays @state) sk video-data)]
              (js/console.log "published (from event:pasted-url)" published)
              (reset! input-value "")
              (swap! state assoc :loading? false))))
        100))))

(defn component:url-input []
  (let [input-value (r/atom "")]
    (fn []
      [:div.url-input
       [:input {:type "text"
                :placeholder "Paste YouTube URL to add"
                :value @input-value
                :on-change #(reset! input-value (.. % -target -value))
                :on-paste #(event:pasted-url state input-value %)}]])))

(defn component:settings-relays [state]
  [:div.setting-group
   [:h3 "Relays"]
   [:p
    "Set the Nostr relays you'd like to use "
    "to sync and store your watch list."]
   (for [[idx relay] (map-indexed vector (:relays @state))]
     ^{:key idx}
     [:row-group
      [:input {:type "text"
               :value relay
               :on-change #(swap! state assoc-in
                                  [:relays idx]
                                  (.. % -target -value))
               :on-blur #(publish-settings! @state)}]
      [:button.icon-button
       {:on-click #(when
                     (js/confirm "Delete this relay?")
                     (swap! state update :relays
                            (fn [relays]
                              (vec (concat
                                     (subvec relays 0 idx)
                                     (subvec relays (inc idx))))))
                     (publish-settings! @state))
        :alt "Delete relay"}
       [icon (load-icon "outline/trash.svg")]]])
   [:button.button
    {:on-click #(swap! state update :relays conj "wss://")}
    [icon (load-icon "outline/plus.svg")]
    " Add relay"]])

(defn event:paste-nsec [decrypting-atom ev]
  (let [pasted-text (.. ev -clipboardData (getData "text"))]
    (js/console.log "pasted-text" pasted-text)
    (.preventDefault ev)
    (reset! decrypting-atom true)
    (p/do!
      ; allow atom swap time to update UI
      (p/delay 1)
      (try
        (if (.startsWith pasted-text "ncryptsec")
          ;; Handle ncryptsec directly
          (when (js/confirm "Are you sure you want to replace the private key?")
            (let [pw (js/prompt "Enter password to decrypt key:")]
              (if (seq pw)
                (p/do!
                  (p/delay 100) ;; Small delay to ensure UI updates
                  (let [decrypted (decrypt-key-with-pw pasted-text pw)]
                    (if decrypted
                      (do
                        (set-key decrypted)
                        (js/window.location.reload))
                      (do
                        (js/alert "Failed to decrypt with provided password")
                        (reset! decrypting-atom false)))))
                (reset! decrypting-atom false))))

          ;; Handle nsec or other formats
          (let [decoded (nostr-decode pasted-text)
                type (.-type decoded)
                data (.-data decoded)]
            (if (js/confirm "Are you sure you want to replace the private key?")
              (if (= type "nsec")
                (do
                  (set-key data)
                  (js/window.location.reload))
                (do
                  (js/alert "Invalid key format")
                  (reset! decrypting-atom false)))
              (reset! decrypting-atom false))))
        (catch :default e
          (js/console.error "Error importing key" e)
          (js/alert "Invalid key format")
          (reset! decrypting-atom false))))))

(defn component:settings-nsec []
  (let [password (r/atom "")
        encrypting? (r/atom false)
        decrypting? (r/atom false)]
    (fn [sk]
      [:div.setting-group
       [:h3 "Account"]
       [:p "Your nsec is the key to access your account."]
       [:p
        "You can sync your watch list to another device,
        or back it up, by copying and saving your nsec key."]
       [:row-group
        [:input {:type "password"
                 :autocomplete "off"
                 :placeholder
                 "(Optional) Enter a password to encrypt your nsec key."
                 :value @password
                 :on-change #(reset! password (.. % -target -value))}]
        [:button.button
         {:disabled @encrypting?
          :on-click #(do
                       (reset! encrypting? true)
                       (js/setTimeout
                         (fn []
                           (p/let [result (if (seq @password)
                                            (encrypt-key-with-pw sk @password)
                                            (nostr-encode-nsec sk))]
                             (js/console.log "encrypted" result)
                             (copy-to-clipboard result)
                             (js/alert (str (if (seq @password)
                                              "ncrypt"
                                              "nsec")
                                            " copied to clipboard!"))
                             (reset! password "")
                             (reset! encrypting? false))) 1))}

         (if @encrypting?
           [component:loading-spinner {:data-size "small"}]
           "Copy")]]
       [:p
        "Restore a watch list or sync with a different device
        by pasting the nsec here:"]
       [:div
        (if @decrypting?
          [component:loading-spinner {:data-size "small"}]
          [:input {:autocomplete "off"
                   :value ""
                   :placeholder
                   (str
                     "Paste nsec/ncrypt here "
                     "to sync up another device.")
                   :on-paste (partial event:paste-nsec decrypting?)}])]])))

(defn component:settings-sync [_state nsec pin-input show-qr]
  [:div.setting-group
   [:h3 "Sync to Device"]
   [:p "You can sync your watch list to another device by scanning a QR code, or
       copying and pasting the nsec."]
   (if @show-qr
     [:div
      [:div#qrcode]
      [:button.button {:on-click
                       #(reset! show-qr false)}
       "Hide QR Code"]]
     [:div
      [:input {:type "password"
               :autocomplete "off"
               :placeholder "Enter a PIN to encrypt the transfer"
               :value @pin-input
               :on-change #(reset! pin-input (.. % -target -value))}]
      [:button.button
       {:on-click #(when (and @pin-input (not= @pin-input ""))
                     (p/let [encrypted-key
                             (encrypt-key-with-pw nsec @pin-input)]
                       (when encrypted-key
                         (let [url (str (.-origin js/window.location)
                                        (.-pathname js/window.location)
                                        "?key=" encrypted-key)]
                           (reset! show-qr true)
                           (js/setTimeout
                             (fn []
                               (js/QRCode. "qrcode" #js {:text url
                                                         :width 256
                                                         :height 256}))
                             100)))))}
       "Generate QR Code"]])])

(defn component:help []
  [:section#help
   [:p
    "Watch Later is an online app for saving a list of YouTube videos "
    "to watch later on."]
   [:p "Simply paste a YouTube video URL in the input above to get started."]
   [:h2 "Your 'watch list'"]
   [:p
    "Your list of videos is saved to servers called 'relays' on the "
    [:a {:href "https://wikipedia.org/wiki/Nostr"
         :target "_BLANK"} "Nostr network"] "."]
   [:p
    "You can access your private watch list from any device. A secret
    'nsec' key has been created for you which secures access to your
    encrypted watch list."]
   [:p
    "You can copy your nsec key from the settings page. "
    "Keep a copy somewhere and you can use it to sync your list
    to other devices."]])

(defn component:settings-panel [state]
  (let [sk (:sk @state)]
    [:div.settings-panel
     [:h2 "Settings"]
     #_ [component:settings-sync state nsec (r/atom "") (r/atom false)]
     [component:settings-nsec sk]
     [component:settings-relays state]
     [:h1 "About"]
     [component:help]]))

(defn component:header [state]
  [:header
   [:nav
    [:h1
     [icon (load-icon "filled/brand-youtube.svg")]
     "Watch Later"]
    [:div {:style {:display "flex" :align-items "center" :gap "8px"}}
     (let [connected-count (:connected-relays @state)
           total-relays (count (:relays @state))]
       [:span (str connected-count "/" total-relays)])
     [:button.icon-button
      {:on-click #(do
                    (when (:settings-open? @state)
                      (publish-settings! @state))
                    (swap! state update :settings-open? not))
       :alt "Settings"}
      [icon
       (if (:settings-open? @state)
         (load-icon "outline/x.svg")
         (load-icon "outline/settings.svg"))]]]]])

(defn component:tabs [state]
  (let [unwatched-count (->> (:videos @state)
                             (filter #(not (:viewed %)))
                             count)
        watched-count (->> (:videos @state)
                           (filter :viewed)
                           count)]
    [:div.tabs
     [:button.tab-button
      {:class (when (= (:active-tab @state) :unwatched) "active")
       :on-click #(swap! state assoc :active-tab :unwatched)}
      "Queue" (when (> unwatched-count 0) (str " (" unwatched-count ")"))]
     [:button.tab-button
      {:class (when (= (:active-tab @state) :watched) "active")
       :on-click #(swap! state assoc :active-tab :watched)}
      "Watched" (when (> watched-count 0) (str " (" watched-count ")"))]]))

(defn component:main-view [state]
  [:div.content
   [component:url-input]

   (when (seq (:videos @state))
     [component:tabs state])

   (if (or (:loading? @state)
             (and
               (nil? (:eose? @state))
               (not (:generated? @state))))
     [component:loading-spinner]
     (when (empty? (:videos @state))
       [component:help]))

   (let [videos-by-viewed (->> (:videos @state)
                               (group-by :viewed)
                               (into {}))
         unwatched (get videos-by-viewed false [])
         watched (get videos-by-viewed true [])
         sorted-unwatched (->> unwatched
                               (sort-by #(* -1 (aget (:event %) "created_at"))))
         sorted-watched (->> watched
                             (sort-by #(* -1 (aget (:event %) "created_at"))))
         current-videos (case (:active-tab @state)
                          :unwatched sorted-unwatched
                          :watched sorted-watched
                          sorted-unwatched)]
     [:div.videos-container
      [:div.videos-section
       [:div.videos-list
        (doall
          (for [video current-videos]
            ^{:key (:url video)}
            [component:video-item (:sk @state) video]))]]])])

(defn app [state]
  [:<>
   [component:header state]
   [:main
    (if (:settings-open? @state)
      [component:settings-panel state]
      [component:main-view state])]
   [component:video-modal]])

;*** launch ***;

(p/let [[sk generated?] (generate-or-load-keys)]
  (let [stored-relays (js/localStorage.getItem localstorage-relays-key)]
    (when stored-relays
      (swap! state assoc :relays (js->clj (js/JSON.parse stored-relays)))))
  (js/console.log
    (-> sk
        (pubkey)
        nostr-encode-npub))
  (let [pool (js/NostrTools.SimplePool.)]
    (swap! state
           (fn [*state]
             (-> *state
                 (assoc
                   :sk sk
                   :generated? generated?
                   :pool pool)
                 (subscribe-to-events
                   sk (:relays @state)
                   ; event decrypted content received
                   #(event-received %1 %2)
                   ; eose received
                   #(swap! state assoc :eose? true))))))
  (check-url-params)
  (js/setInterval #(check-connected-relays! state) 1000)
  (wait-for-preload)
  (rdom/render [app state] (.getElementById js/document "app")))
