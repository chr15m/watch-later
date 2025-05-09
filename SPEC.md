Watch Later is a simple one page Scittle/Reagent/ClojureScript app to store a list of YouTube links to watch later.

The design is a super minimal simple vertical feed which works on mobile and desktop.

The nostr-tools library is available at `js/NostrTools` (i.e. on the window object).

Nip implementations are available at e.g. `js/NostrTools.nip04.encrypt`.

When the user loads the page a Nostr private key is generated and stored in localStorage, or loaded from localStorage if it already exists.

A subscription is started on the "app data" feed for the corresponding public key with a #d field of `cx.mccormick.watchlater`.

The user interface has an input at the top where a YouTube link can be pasted.

When pasted, the youtube link is turned into an event and posted to Nostr as a new "app data" event with the above properties.

A `p/let` causes the app to wait for the publish to at least one relay and a loading spinner is shown until that happens.

The event's content field contains a nip04 encrypted JSON payload:

```json
{
  "url": "...youtube url that was pasted...",
  "useragent": js/navigator.userAgent,
  "viewed": false,
}
```

The #a tag should contain not just 30078, pk, and d, but also the first 8 hex characters of the hash of the URL so it can be replaced once the user has marked it as viewed.

The feed of all links previously posted is listed below the input box at the top.

Events (posts) with a content field where "viewed" is true should have reduced opacity.

Each YouTube video is represented by the thumbnail image from `https://i3.ytimg.com/vi/YOUTUBE-SLUG/mqdefault.jpg`.

If the thumbnail is clicked the youtube link opens in a new tab.

Below each thumbnail there is an eye icon from `https://cdn.jsdelivr.net/npm/@tabler/icons@3.31.0/icons/filled/eye.svg` and `https://cdn.jsdelivr.net/npm/@tabler/icons@3.31.0/icons/outline/eye.svg` which toggles the "viewed" setting for that event and re-publishes it with the updated state.

## Settings

In the top right corner of the app is a settings cog icon from: `https://cdn.jsdelivr.net/npm/@tabler/icons@3.31.0/icons/outline/settings.svg`.

Tapping the cog turns the icon into an X: `https://cdn.jsdelivr.net/npm/@tabler/icons@3.31.0/icons/outline/x.svg` and opens the settings page.

The settings page lists two default relays in input boxes and these can be modified.

The settings page contains a "copy nsec" button which copies the nsec to the clipboard.

The settings page contains an input box with the placeholder "nsec" and if an nsec is pasted in there then it replaces the existing nsec in localStorage and the app reloads - a confirmation alert is shown before this takes place.

## Sharing

In the settings page there is a "sync to device" button.

When clicked the user is prompted to enter a pin.

The nsec is then encrypted with nip49 Private Key Encryption using the pin and a QR code (`js/QRCode`) is shown with a link to the current page with `?key=...encrypted-key...`.

When the user scans the QR code on another device and the page is opened, the `?key` is read from the search query and the user is prompted to enter the pin.

Once the key is decrypted it's set in localStorage so that this new device reads and writes YouTube URLs from the same Nostr event feed.
