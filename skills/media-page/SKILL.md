---
name: media-page
description: Present screenshots or video as one self-contained HTML page. Use when a reader must see a result — UI changes, demo recordings, visual reviews.
---

`media-page` turns a short source file plus existing media into one HTML page that opens in the DSH sidebar and any browser. Its `--help` is the format reference; run it before writing a source file.

1. Collect the media. Recording and screenshots come from the project's own tooling. Keep each video to the moment that matters, and a page's video to about one to three minutes in total.
2. Write the source file in the directory that holds the media, so its paths stay relative. Give every image and video a caption that states what the reader sees and why it matters; caption only what you verified.
3. Run `media-page FILE`. Each error names `FILE:LINE`; fix the source and rerun until the command prints a path.
4. Present the printed path to the user (`present` in DSH), with a short summary of what the page shows.
