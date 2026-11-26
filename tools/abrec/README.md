# abrec

> **Status:** phase 1 (library, rendering, admin site, NixOS module) is
> implemented. Browser recording (phase 2) is design only. This document is the
> working spec; change it before changing code.

Record yourself reading a book aloud, one chapter per take, and publish the
chapters as private podcast feeds that a listener follows once in a podcast
app. The app provides locked-screen playback, resume, sleep timer, offline
download and continuing to the next chapter; abrec does not ship a player.

## Pieces

- **`abrec`**: one Haskell web service. It serves the admin site, records and
  stores takes, and renders the public site.
- **Admin site**: management and in-browser recording. Reachable only on a
  private network chosen by the operator; it has no login.
- **Public site**: static files rendered by abrec and served by nginx. Nothing
  public reaches the Haskell process.
- **`nixosModules.abrec`**: the service, its state directory and both nginx
  virtual hosts. TLS, DNS and network restriction are the consumer's choice.

## Admin site

Plain HTML forms (post, then redirect). htmx updates the publish status.
Hand-written client code exists only on the record page.

- **Library**: books with chapter counts; New book; library title (default
  "Audiobooks") for the library feed.
- **Book**: title (required), author, description, cover. Chapters in order,
  each with a player, episode title, recorded time, and Edit · Renumber ·
  Replace · Hide · Delete. **Record chapter N** (N = next chapter) and
  **Upload files**. Missing chapter numbers (1, 2, 4) are flagged.
- **Chapter**: an optional title. The episode title defaults to
  `<N>. <title>` (`13. Fire and Water`), or `Chapter <N>` without a title.
  Free text replaces the default; clearing it restores the default, which then
  follows renumbering.
- **Hidden**: books and chapters have a hidden switch, off by default. Hidden
  items stay in the admin site but are left out of every feed and the listener
  page; a hidden book's feed URL returns 404 until it is shown again, at the
  same URL. Hiding a chapter removes it from listeners' feeds, though copies
  already downloaded may remain on their devices.
- **Delete** asks for confirmation, then removes the master and its metadata
  permanently. There is no trash.
- **Cover**: upload an image or paste an HTTPS URL; the server fetches it,
  checks it is an image and converts it to a square 1400–3000 px JPEG. Without
  a cover the book gets a generated title card.
- **Upload files**: one or more audio files (for example Audacity exports).
  Chapter numbers default from a leading number in each filename and can be
  edited before saving. Existing chapters are replaced only after confirming.
- **Links**: listener URLs for each book and the library page; Rotate secret.
- **Publish status**: every page shows whether the public site is current,
  publishing, or failed (with the error), refreshed by htmx while publishing.

### Recording

The record page targets current desktop browsers and Chrome on Android. iOS
Safari is not a target.

    The Hobbit — Chapter 13   Title (optional): [Fire and Water      ]
    ● 00:23:41   ▮▮▮▮▮▮▯▯▯   [Pause] [Stop]
    Saved on server: 00:23:38

- **Input device**: a microphone picker, remembered by the browser.
- `getUserMedia` with echo cancellation, noise suppression and automatic gain
  control disabled. `MediaRecorder` records at 128 kb/s in the browser's
  preferred format (Opus in WebM, or AAC in MP4).
- **Chunks during the take**: the recorder emits a chunk every 5 seconds and
  posts it with its index. The server appends chunks in order and ignores a
  repeated index, so retries are safe. The page shows how much the server
  holds. A crashed tab, lost network or flat battery loses seconds, not the
  chapter; reopening the book offers to resume or save the unfinished take.
- **Segments**: a take is one or more segments, each from one recorder
  session. Resuming an unfinished take starts a new segment; Save joins the
  segments with ffmpeg.
- **Pause** uses `MediaRecorder.pause` and stays in the current segment.
- **Level meter** from a Web Audio analyser. After 10 s below −50 dBFS the page
  warns that there is no signal.
- **Wake lock**: the page holds a screen wake lock while recording so a phone
  does not sleep mid-take.
- **Stop** shows the duration with Save and Discard. Save turns the take into
  the chapter's master, returns at once, and publishing continues in the
  background.

## Library

    <libraryDir>/                          0711, owned by the abrec user
      library.json                         library title; next season number
      secret                               URL secret
      books/<slug>/book.json               title; season; hidden; created; optional
                                           author, description, cover hash
      books/<slug>/cover.jpg               optional, 1400 px square
      books/<slug>/chapters/<id>.json      number; optional title and episode title;
                                           hidden; recorded time; source; master
      books/<slug>/masters/<sha256>.<ext>  audio as received
      incoming/                            staged uploads awaiting chapter numbers
      takes/<id>/<segment>/                chunks of a take in progress (phase 2)
      tmp/                                 request bodies and scratch files
      cache/                               encodes and title cards, keyed by hash
      releases/<id>/<secret>/…             rendered sites (0755)
      site -> releases/<id>                the only path nginx serves

- A book's slug and season number are fixed when the book is created.
  Renaming changes the title only, so feed URLs and library order never
  change. Season numbers are never reused after a book is deleted.
- A chapter has a stable random identifier; renumbering edits its JSON. Masters
  are named by hash, so replacing audio never overwrites a file in use, and a
  master is deleted once no chapter refers to it.
- Files are the source of truth; there is no database. Every read and write
  holds one lock, and each JSON file is replaced by renaming a complete file.
- Masters are kept as received, so a later preset change re-encodes from the
  best available audio.
- abrec makes no backups. The masters exist only in `libraryDir`; protecting
  it is the operator's choice.

## Render

Render rebuilds the public site from the library's shown books and chapters.
It runs in the background on one worker: a change during a render queues one
more render, so the site always catches up with the latest library.

- **Encoding**: mono MP3, 64 kb/s CBR, two-pass `loudnorm` to Apple's spoken
  word target (−16 LUFS integrated, true peak below −1 dBTP), leading and
  trailing silence trimmed. Cached by master hash and preset identifier.
- **Title card**: for books without a cover, the title (and author) on a plain
  background, rendered with ffmpeg and a font from the Nix package. The
  library feed gets one with the library title.
- **Feeds** are RSS 2.0 with iTunes tags, `itunes:type` `serial` and
  `itunes:block` `yes`. Publication dates are synthetic (book creation time +
  chapter number minutes) so order never depends on upload time. A chapter's
  GUID includes its master hash, so a replaced chapter downloads again.
  - **Book feed** (`<secret>/<slug>/feed.xml`): one show per book, with the
    book's cover or title card; `itunes:episode` = chapter number; episode title
    as configured.
  - **Library feed** (`<secret>/feed.xml`): one show for every book;
    `itunes:season` = the book's season, `itunes:episode` = chapter number,
    episode title `<book title> · <episode title>`, and the book's cover as
    episode artwork. A new book appears without following again.
- **Listener page** (`<secret>/index.html` and per book): books, chapters with
  `<audio preload="none">` players, a Follow link per book and a Follow
  everything link for the library feed, explaining that a listener should use
  one or the other. `noindex`.
- **Publish**: write `releases/<id>` (files hard-linked from `cache/`), then
  replace `site` with an atomic symlink rename. The previous release is kept;
  older ones are removed. A failed render leaves the current site serving.

Rotating the secret invalidates every listener URL; listeners follow again.

## Security

- **Public site**: static files only. Anyone with a listener URL can listen;
  nothing public accepts input.
- **Admin site**: anyone who can reach it can change the library. The consumer
  restricts it to a private network. abrec additionally:
  - rejects requests whose `Host` is not the configured admin domain (DNS
    rebinding);
  - rejects every non-GET request unless `Sec-Fetch-Site` is `same-origin` or
    `Origin` is the admin origin, so a public web page opened on a private
    network device cannot submit to it;
  - fetches cover URLs over HTTPS only, with size and time limits, and keeps the
    result only if it is an image.

  Reaching the admin site is the only access control; deletion is permanent.
- **Listener URLs**: some podcast apps fetch feeds through their maker's
  servers, so a listener's app provider may see the secret URL. Rotating the
  secret is the remedy.
- **Service**: a dedicated system user, listening on localhost, writing only
  its state directory, under systemd hardening.

## Nix interface

    services.abrec = {
      enable = true;
      adminDomain = "abrec.example.com";
      publicDomain = "audiobooks.example.com";
      # port = 8790;          admin site on 127.0.0.1
      # maxUploadSize = "4g"; nginx limit for admin requests
      # libraryDir is /var/lib/abrec (read-only option, StateDirectory)
    };

The module defines `services.nginx.virtualHosts.<adminDomain>` (proxy with
`Host` forwarded, large unbuffered uploads, strict CSP) and `.<publicDomain>`
(static files from `site`, no listing, dotfiles denied, RSS content type). Add
TLS and listen addresses by merging into those virtual hosts.

The service runs `abrec serve CONFIG` as the `abrec` user under systemd
hardening; `abrec render CONFIG` rebuilds the public site once from a shell
(`sudo -u abrec`). `package.nix` passes ffmpeg-headless, `ffprobe`, `curl`, the
title-card font and the static directory (admin CSS and htmx) through
`wrapTool`.

## Stack

- warp and scotty; HTML from `ihp-hsx` (HSX without the rest of IHP); aeson for
  the library files.
- htmx, vendored at a pinned version through Nix, for status updates without
  hand-written client code.
- Hand-written client code is TypeScript, only where a browser API requires it
  (the recorder). `record.ts` uses DOM types only and is compiled by nixpkgs'
  `typescript` during the package build: no npm, no `node_modules`, no bundler.
  The compiled file is served by abrec with the other static assets. A flake
  check runs `tsc --noEmit` in strict mode.

## Tests

In-process HTTP tests against the WAI application, running the real
`ffmpeg` and `ffprobe` on generated tones and images, with a fake `curl`:

- book and chapter CRUD; slug stable across renames; replace needs
  confirmation;
- chunked takes: in-order append, repeated index ignored, gap rejected,
  unfinished take resumable as a new segment, segments joined on save, discard
  removes it;
- hidden books and chapters absent from feeds and listener pages, present in
  admin; showing a book restores the same URL; delete removes files;
- publishing: save returns before render finishes; a change during a render
  queues exactly one more; status reports current, publishing and failed;
- uploads: chapter numbers from filenames; non-audio rejected;
- covers: upload and URL, non-images and non-HTTPS rejected, title card when
  absent;
- render: feed order, synthetic dates, GUID change on replace, cache reuse,
  previous release kept on failure;
- episode titles: default with and without a chapter title, free-text
  override, default follows renumbering; library feed seasons stable across
  renames and deletions;
- security: wrong `Host` rejected; cross-site and missing-origin writes
  rejected.

A flake check evaluates the NixOS module in a minimal configuration. The
recorder is tested by hand in the target browsers.

## Open questions

- Chrome on Android: does recording continue if the screen turns off despite
  the wake lock, or should the page say to keep it on?
- On a locked iPhone, in Apple Podcasts: does a serial feed continue to the
  next chapter, stop at end of chapter with the sleep timer, and keep order?
- Which apps a `podcast://` link on the listener page opens (labelled "Open in
  Podcast App"): Apple Podcasts on iOS by default, but other apps may claim it,
  and it may need plain HTTP to redirect to HTTPS.
- How Apple Podcasts presents the library feed's seasons and episode artwork.
- How soon new chapters appear: the podcast app chooses when to refresh a feed.

## Phases

1. Library, render, admin pages (books, chapters, uploads, covers, links) and
   the NixOS module: existing recordings become feeds; test on a phone.
2. Browser recording: chunks, pause, meter, wake lock, resume.
3. Listener page polish.
