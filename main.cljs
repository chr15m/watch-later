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

;; Constants
(def app-name "cx.mccormick.watchlater")
(def default-relays ["wss://relay.damus.io"
                     "wss://relay.nostr.band"])
(def icon-url
  "https://cdn.jsdelivr.net/npm/@tabler/icons@3.31.0/icons/")
(def localstorage-key
  "watch-later-nostr-key")
(def nostr-kind 30078)

(set-svg-base-url icon-url)

; TODO:
; - cache stored events and only request since last posted

;; State management
(defonce state (r/atom {:loading? false
                        :videos []
                        :settings-open? false
                        :relays default-relays
                        :qr-code nil
                        :pin nil
                        :sk nil}))

;; Utility functions

(defn pubkey [sk]
  (js/NostrTools.getPublicKey sk))

(defn set-key [sk]
  (->> sk
       js/NostrTools.nip19.nsecEncode
       (js/localStorage.setItem localstorage-key))
  sk)

(defn generate-or-load-keys []
  (let [stored-key (js/localStorage.getItem localstorage-key)
        decoded-sk (try (js/NostrTools.nip19.decode stored-key)
                        (catch :default _e nil))]
    (or (and decoded-sk
             (= (aget decoded-sk "type") "nsec")
             (aget decoded-sk "data"))
        (set-key (js/NostrTools.generateSecretKey)))))

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

(defn encrypt-content [sk pk content]
  (js/NostrTools.nip04.encrypt sk pk (js/JSON.stringify (clj->js content))))

(defn decrypt-content [sk pk encrypted-content]
  (try
    (let [decrypted (js/NostrTools.nip04.decrypt sk pk encrypted-content)]
      (js->clj (js/JSON.parse decrypted) :keywordize-keys true))
    (catch :default e
      (js/console.error "Failed to decrypt content" e)
      nil)))

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

(defn handle-event [event]
  (js/console.log "handle-event" event)
  (js/console.log "id:" (aget event "id"))
  (let [sk (generate-or-load-keys)
        decrypted (decrypt-content sk (pubkey sk) (.-content event))]
    (js/console.log "handle-event decrypted content" (clj->js decrypted))
    (when decrypted
      (swap! state update :videos
             (fn [videos]
               (let [existing-index
                     (first (keep-indexed
                              (fn [idx v]
                                (when (= (:url v) (:url decrypted)) idx))
                              videos))]
                 (if existing-index
                   (assoc-in videos [existing-index]
                             (assoc decrypted :event event))
                   (conj (or videos [])
                         (assoc decrypted :event event)))))))))

(defn subscribe-to-events [pk relays]
  (let [pool (js/NostrTools.SimplePool.)
        sub (.subscribeMany pool
                            (clj->js relays)
                            (clj->js [{:kinds [nostr-kind]
                                       :authors [pk]
                                       :#n [app-name]}])
                            (clj->js {:onevent handle-event}))]
    sub))

(defn toggle-viewed [video]
  (swap! state assoc :loading? true)
  (let [sk (generate-or-load-keys)
        uuid (:uuid video)
        url (:url video)
        new-viewed (not (:viewed video))
        metadata (:metadata video)]
    (p/let [hash-fragment (hash-url url)
            event (create-event sk (pubkey sk)
                                url new-viewed hash-fragment metadata uuid)]
      (publish-event event (:relays @state))
      (swap! state assoc :loading? false))))

(defn copy-to-clipboard [text]
  (let [el (.createElement js/document "textarea")]
    (set! (.-value el) text)
    (.appendChild (.-body js/document) el)
    (.select el)
    (.execCommand js/document "copy")
    (.removeChild (.-body js/document) el)))

(defn encrypt-key-with-pin [nsec pin]
  (try
    (js/NostrTools.nip49.encryptPrivateKey nsec pin)
    (catch :default e
      (js/console.error "Failed to encrypt key" e)
      nil)))

(defn decrypt-key-with-pin [encrypted-key pin]
  (try
    (let [result (js/NostrTools.nip49.decryptPrivateKey encrypted-key pin)]
      (.-data result))
    (catch :default e
      (js/console.error "Failed to decrypt key" e)
      nil)))

;; Components
(defn loading-spinner []
  [:div.loading [:div]])

(defn video-item [{:keys [url viewed uuid event metadata]}]
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
         {:on-click #(toggle-viewed {:url url
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
          (let [sk (generate-or-load-keys)
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

(defn url-input []
  (let [input-value (r/atom "")]
    (fn []
      [:div.url-input
       [:input {:type "text"
                :placeholder "Paste YouTube URL here"
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
     [:input {:type "text"
              :value relay
              :on-change #(swap! state assoc-in
                                 [:relays idx]
                                 (.. % -target -value))}])])

(defn component:settings-nsec []
  (let [password (r/atom "")]
    (fn [_state nsec nsec-input]
      [:div.setting-group
       [:h3 "Account"]
       [:p
        "You can sync your watch list to another account, or back it up by saving
        your nsec key."]
       [:row-group
        [:input {:type "password"
                 :placeholder
                 "(Optional) Enter a password to encrypt your nsec key."
                 :value @password
                 :on-change #(reset! password (.. % -target -value))}]
        [:button.button
         {:on-click #(p/let [result (if (seq @password)
                                      (encrypt-key-with-pin nsec @password)
                                      nsec)]
                       (copy-to-clipboard result)
                       (js/alert (str (if (seq @password)
                                        "ncrypt"
                                        "nsec")
                                      " copied to clipboard!")))}
         "Copy"]]
       [:p
        "Restore a watch list or sync with a different device
        by pasting the nsec here."]
   [:div
    [:input {:type "password"
             :placeholder "Paste nsec/ncrypt here to sync up another device."
             :value @nsec-input
             :on-change #(reset! nsec-input (.. % -target -value))}]
    [:button.button
     {:on-click
      #(when
         (js/confirm "Are you sure you want to replace the private key?")
         (try
           (let [decoded (js/NostrTools.nip19.decode @nsec-input)
                 type (.-type decoded)
                 data (.-data decoded)]
             (cond
               (= type "nsec")
               (do
                 (set-key data)
                 (js/window.location.reload))

               (= type "ncryptsec")
               (let [pin (js/prompt "Enter PIN to decrypt key:")]
                 (when (and pin (not= pin ""))
                   (let [decrypted (decrypt-key-with-pin @nsec-input pin)]
                     (if decrypted
                       (do
                         (set-key decrypted)
                         (js/window.location.reload))
                       (js/alert "Failed to decrypt with provided PIN")))))

               :else
               (js/alert "Invalid key format")))
           (catch :default e
             (js/console.error "Error importing key" e)
             (js/alert "Invalid key format"))))}
     "Import Key"]]])))

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
               :placeholder "Enter a PIN to encrypt the transfer"
               :value @pin-input
               :on-change #(reset! pin-input (.. % -target -value))}]
      [:button.button
       {:on-click #(when (and @pin-input (not= @pin-input ""))
                     (p/let [encrypted-key
                             (encrypt-key-with-pin nsec @pin-input)]
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

(defn component:settings-panel [state]
  (let [sk (generate-or-load-keys)
        nsec (js/NostrTools.nip19.nsecEncode sk)]
    [:div.settings-panel
     [:h2 "Settings"]
     #_ [component:settings-sync state nsec (r/atom "") (r/atom false)]
     [component:settings-nsec state nsec (r/atom "")]
     [component:settings-relays state]]))

(defn check-url-params []
  (let [url-params (js/URLSearchParams. (.-search js/window.location))
        key-param (.get url-params "key")]
    (when key-param
      (let [pin (js/prompt "Enter PIN to decrypt key:")]
        (when (and pin (not= pin ""))
          (let [decrypted (decrypt-key-with-pin key-param pin)]
            (when decrypted
              (let [pk (js/NostrTools.getPublicKey decrypted)
                    keys-obj #js {:sk decrypted :pk pk}]
                (js/localStorage.setItem "nostr-key"
                                         (js/JSON.stringify keys-obj))
                (js/window.location.reload)))))))))

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
   [url-input]

   (when (:loading? @state)
     [loading-spinner])

   (let [[unwatched watched]
         (->> (:videos @state)
              (group-by
                :viewed)
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
            [video-item video]))]]

      (when (seq watched)
        [:div.videos-section
         [:h3 "Watched"]
         [:div.videos-list
          (doall
            (for [video watched]
              ^{:key (:url video)}
              [video-item video]))]])])])

(defn app [state]
  [:<>
   [component:header state]
   [:main
    (if (:settings-open? @state)
      [component:settings-panel state]
      [component:main-view state])]])

;*** launch ***;

(p/let [sk (generate-or-load-keys)]
  (js/console.log
    (-> sk
        (pubkey)
        js/NostrTools.nip19.npubEncode))
  (swap! state assoc :sk sk)
  (check-url-params)
  (subscribe-to-events (pubkey sk) (:relays @state))
  (wait-for-preload)
  (rdom/render [app state] (.getElementById js/document "app")))
