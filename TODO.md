# Current dev

## TODO

- [ ] Cache locally and use "since" flag in fetch.
- [ ] Bug: Make client authoritative if e.g. 'watched' button pressed.
- [ ] Ability to add by URL with bookmarklet (`SPEC-add-url.md`).
- [ ] Relay management:
  - [x] Highlight non-connected relays in warning color in the settings page.
  - [ ] Track relay publish/connect error counts.
  - [ ] Suggest refreshing any relay which has gone bad.
  - [ ] Pick deterministic relays using pretty-good-relays library.

## Next

- [ ] Pre-load video in an iframe to get total time and display it
- [ ] Fix inconsistent use of :url and :uuid

## Done

- [x] Bug: add-to-homescreen on Firefox does not create a full screen "app".
- [x] Get QR auth sync working.
- [x] Top right corner close button hides interface during video playback.
- [x] Show current npub in settings
- [x] Generate PWA manifest dynamically, remove sw.js and manifest.
- [x] Make colors match other apps
- [x] Mobile title - shrink to fit
- [x] Bug: pressing any button opens video

