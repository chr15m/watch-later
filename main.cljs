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
                        :pin nil}))

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
  [:div.loader])

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
       [:img.thumbnail {:src thumbnail-url :alt "Video thumbnail"}]
       [:div.video-title title]]]
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
          (load-icon "outline/eye.svg"))]]]]))

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

(defn settings-panel []
  (let [nsec-input (r/atom "")
        pin-input (r/atom "")
        show-qr (r/atom false)]
    (fn []
      (let [sk (generate-or-load-keys)
            nsec (js/NostrTools.nip19.nsecEncode sk)]
        [:div.settings-panel
         [:h2 "Settings"]

         [:div.setting-group
          [:h3 "Relays"]
          (for [[idx relay] (map-indexed vector (:relays @state))]
            ^{:key idx}
            [:input {:type "text"
                     :value relay
                     :on-change #(swap! state assoc-in
                                        [:relays idx]
                                        (.. % -target -value))}])]

         [:div.setting-group
          [:h3 "Account"]
          [:button.button
           {:on-click #(do (copy-to-clipboard nsec)
                           (js/alert "nsec copied to clipboard!"))}
           "Copy nsec"]

          [:div
           [:input {:type "text"
                    :placeholder "Paste nsec here to import"
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
                    (if (= type "nsec")
                      (do
                        (set-key data)
                        (js/window.location.reload))
                      (js/alert "Invalid nsec format")))
                  (catch :default e
                    (js/console.error "Error importing nsec" e)
                    (js/alert "Invalid nsec format"))))}
            "Import Key"]]]

         [:div.setting-group
          [:h3 "Sync to Device"]
          (if @show-qr
            [:div
             [:div#qrcode]
             [:button.button {:on-click
                              #(reset! show-qr false)}
              "Hide QR Code"]]
            [:div
             [:input {:type "password"
                      :placeholder "Enter PIN for encryption"
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
              "Generate QR Code"]])]]))))

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

(defn app []
  (let [sk (generate-or-load-keys)]
    (r/create-class
      {:component-did-mount
       (fn []
         (check-url-params)
         (subscribe-to-events (pubkey sk) (:relays @state)))

       :reagent-render
       (fn []
         [:main
          [:div.app-header
           [:h1 "Watch Later"]
           [:button.icon-button
            {:on-click #(swap! state update :settings-open? not)
             :alt "Settings"}
            [icon
             (if (:settings-open? @state)
               (load-icon "outline/x.svg")
               (load-icon "outline/settings.svg"))]]]

          (if (:settings-open? @state)
            [settings-panel]
            [:div.content
             [url-input]

             (when (:loading? @state)
               [loading-spinner])

             [:div.videos-list
              (doall
                (for [video (sort-by (fn [video]
                                       [(:viewed video)
                                        (* -1 (aget (:event video)
                                                    "created_at"))])
                                     (:videos @state))]
                  ^{:key (:url video)}
                  [video-item video]))]])])})))

(js/console.log
  (->
    (generate-or-load-keys)
    (pubkey)
    js/NostrTools.nip19.npubEncode))

(p/do!
  (wait-for-preload)
  (rdom/render [app] (.getElementById js/document "app")))
