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
; - save relay list to nostr
; - when the eye is clicked show a loading spinner in the button
; - when the ncrypt input is pasted show a loading spinner
; - fix the giant spinner when clicking a button
; - work out a good set of default relays
; - create a basic README

; TODO (stretch goals)
; - use the yt api to play in a modal, track playback, and store playback time
; - cache stored events and only request since last posted

; immutable constant data

(def app-name "cx.mccormick.watchlater")
(def default-relays ["wss://relay.damus.io"
                     "wss://relay.nostr.band"])
(def icon-url
  "https://cdn.jsdelivr.net/npm/@tabler/icons@3.31.0/icons/")
(def localstorage-key
  "watch-later-nostr-key")
(def nostr-kind 30078)

; mutable state data

(set-svg-base-url icon-url)

(defonce state (r/atom {:loading? false
                        :videos []
                        :settings-open? false
                        :relays default-relays
                        :generated? nil
                        :sk nil}))

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

(defn encrypt-content [sk pk content]
  (js/NostrTools.nip04.encrypt sk pk (js/JSON.stringify (clj->js content))))

(defn decrypt-content [sk pk encrypted-content]
  (try
    (let [decrypted (js/NostrTools.nip04.decrypt sk pk encrypted-content)]
      (js->clj (js/JSON.parse decrypted) :keywordize-keys true))
    (catch :default e
      (js/console.error "Failed to decrypt content" e)
      nil)))

(defn create-event [sk pk url viewed hash-fragment metadata & [existing-uuid]]
  (js/console.log "create-event"
                  sk pk url viewed hash-fragment metadata existing-uuid)
  (let [uuid (or existing-uuid (str (random-uuid)))
        content {:uuid uuid
                 :url url
                 :useragent (.-userAgent js/navigator)
                 :viewed viewed
                 :metadata metadata}
        encrypted-content (encrypt-content sk pk content)
        event-template
        #js {:kind nostr-kind
             :created_at (js/Math.floor (/ (js/Date.now) 1000))
             :tags #js [#js ["d" (str app-name ":" uuid)]
                        #js ["n" app-name]]
             :content encrypted-content}]
    (js/console.log "event-template" event-template)
    (js/console.log "content" (clj->js content))
    (js/NostrTools.finalizeEvent event-template sk)))

(defn publish-event [event relays]
  (js/console.log "publish-event" event relays)
  (p/let [pool (js/NostrTools.SimplePool.)
          published (js/Promise.any (.publish pool (clj->js relays) event))]
    published))

(defn subscribe-to-events [sk pk relays event-callback eose-callback]
  (let [pool (js/NostrTools.SimplePool.)
        sub (.subscribeMany
              pool
              (clj->js relays)
              (clj->js [{:kinds [nostr-kind]
                         :authors [pk]
                         :#n [app-name]}])
              (clj->js {:onevent
                        (fn [event]
                          (let [decrypted-content
                                (decrypt-content
                                  sk (pubkey sk)
                                  (.-content event))]
                            (when decrypted-content
                              (event-callback decrypted-content event))))
                        :oneose eose-callback}))]
    sub))

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

;*** components and events ***;

(defn component:loading-spinner []
  [:div.loading [:div]])

(defn event:toggle-viewed [sk video]
  (swap! state assoc :loading? true)
  (let [uuid (:uuid video)
        url (:url video)
        new-viewed (not (:viewed video))
        metadata (:metadata video)]
    (p/let [hash-fragment (hash-url url)
            event (create-event sk (pubkey sk)
                                url new-viewed hash-fragment metadata uuid)]
      (publish-event event (:relays @state))
      (swap! state assoc :loading? false))))

(defn component:video-item [sk {:keys [url viewed uuid event metadata]}]
  (js/console.log "video-item render" url viewed)
  (let [youtube-id (get-youtube-id url)
        thumbnail-url (get-thumbnail-url youtube-id)
        title (if (and metadata (:title metadata))
                (:title metadata)
                "YouTube Video")]
    [:div.video-item {:class (when viewed "viewed")}
     [:div.thumbnail-container
      [:a {:href url :target "_blank"}
       [:img.thumbnail {:src thumbnail-url :alt "Video thumbnail"}]]
      [:row-group
       [:div.video-title title]
       [:div.video-controls
        [:button.icon-button
         {:on-click #(event:toggle-viewed
                       sk
                       {:url url
                        :viewed viewed
                        :uuid uuid
                        :event event
                        :metadata metadata})
          :alt (if viewed "Viewed" "Mark as viewed")}
         [icon
          (if viewed
            (load-icon "filled/eye.svg")
            (load-icon "outline/eye.svg"))]]]]]]))

(defn event:pasted-url [state input-value ev]
  (let [pasted-text (.. ev -clipboardData (getData "text"))]
    (when (get-youtube-id pasted-text)
      (reset! input-value pasted-text)
      (js/setTimeout
        (fn []
          (let [sk (:sk @state)
                youtube-id (get-youtube-id pasted-text)]
            (swap! state assoc :loading? true)
            (p/let [hash-fragment (hash-url pasted-text)
                    metadata (fetch-youtube-metadata youtube-id)
                    event (create-event
                            sk (pubkey sk)
                            pasted-text false hash-fragment metadata)
                    published (publish-event event (:relays @state))]
              (js/console.log "published" published)
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
                                  (.. % -target -value))}]
      [:button.icon-button
       {:on-click #(swap! state update :relays
                          (fn [relays]
                            (vec (concat
                                  (subvec relays 0 idx)
                                  (subvec relays (inc idx))))))
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
        (reset! decrypting-atom false)))))

(defn component:settings-nsec []
  (let [password (r/atom "")
        encrypting? (r/atom false)]
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
                             (reset! encrypting? false))) 1))}
         (if @encrypting?
           [component:loading-spinner]
           "Copy")]]
       [:p
        "Restore a watch list or sync with a different device
        by pasting the nsec here:"]
       (let [decrypting? (r/atom false)]
         [:div
          (if @decrypting?
            [component:loading-spinner]
            [:input {:autocomplete "off"
                     :value ""
                     :placeholder
                     (str
                       "Paste nsec/ncrypt here "
                       "to sync up another device.")
                     :on-paste (partial event:paste-nsec decrypting?)}])])])))

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
    [:button.icon-button
     {:on-click #(swap! state update :settings-open? not)
      :alt "Settings"}
     [icon
      (if (:settings-open? @state)
        (load-icon "outline/x.svg")
        (load-icon "outline/settings.svg"))]]]])

(defn component:main-view [state]
  [:div.content
   [component:url-input]

   (if (or (:loading? @state)
             (and
               (nil? (:eose? @state))
               (not (:generated? @state))))
     [component:loading-spinner]
     (when (empty? (:videos @state))
       [component:help]))

   (let [[unwatched watched]
         (->> (:videos @state)
              (group-by
                :viewed)
              (sort-by first)
              (map second)
              (map
                #(sort-by
                   (fn [video]
                     (* -1 (aget (:event video)
                                 "created_at"))) %)))]
     [:div.videos-container
      [:div.videos-section
       [:div.videos-list
        (doall
          (for [video unwatched]
            ^{:key (:url video)}
            [component:video-item (:sk @state) video]))]]

      (when (seq watched)
        [:div.videos-section
         [:h3 "Watched"]
         [:div.videos-list
          (doall
            (for [video watched]
              ^{:key (:url video)}
              [component:video-item (:sk @state) video]))]])])])

(defn app [state]
  [:<>
   [component:header state]
   [:main
    (if (:settings-open? @state)
      [component:settings-panel state]
      [component:main-view state])]])

;*** launch ***;

(p/let [[sk generated?] (generate-or-load-keys)]
  (js/console.log
    (-> sk
        (pubkey)
        nostr-encode-npub))
  (swap! state assoc
         :sk sk
         :generated? generated?)
  (check-url-params)
  (subscribe-to-events
    sk (pubkey sk) (:relays @state)
    ; event decrypted content received
    (fn [decrypted-content event]
      (swap! state update :videos
              (fn [videos]
                (let [existing-index
                      (first (keep-indexed
                               (fn [idx v]
                                 (when (= (:url v) (:url decrypted-content)) idx))
                               videos))]
                  (if existing-index
                    (assoc-in videos [existing-index]
                              (assoc decrypted-content :event event))
                    (conj (or videos [])
                          (assoc decrypted-content :event event)))))))
    ; eose received
    #(swap! state assoc :eose? true))
  (wait-for-preload)
  (rdom/render [app state] (.getElementById js/document "app")))
