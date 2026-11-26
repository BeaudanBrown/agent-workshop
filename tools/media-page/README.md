# media-page

Builds one self-contained HTML page from a short Markdown-style source file,
embedding every image and video:

    media-page notes.md

The command prints the page's path. `media-page --help` documents the source
format with a complete example; errors are reported as `FILE:LINE`.

Pages work without scripts: media is embedded as data URIs and tabs switch with
CSS, so they render in the DSH sidebar's static HTML preview as well as in a
browser. Media is re-encoded with ffmpeg (H.264 MP4 video, WebP images) and
cached beside the pages.

## Configuration

| Variable | Meaning | Default |
| --- | --- | --- |
| `MEDIA_PAGE_OUT_DIR` | Directory for pages and the encoding cache | `/tmp/media-page` |
| `MEDIA_PAGE_MAX_BYTES` | Largest page written, in bytes | `67108864` (64 MiB) |
| `MEDIA_PAGE_FFMPEG`, `MEDIA_PAGE_FFPROBE` | Set by the Nix wrapper | — |

`-o PATH` writes one page elsewhere; the cache stays in `MEDIA_PAGE_OUT_DIR`.
Keep the size cap at or below the DSH `workspace-files.maxFileBytes` setting,
or the sidebar refuses pages the tool accepted.

## Development

The parser accepts only the documented grammar. A new feature is a new
directive name in `Workshop.MediaPage.Parse.containerDirectives` plus its
rendering in `Workshop.MediaPage.Render`; the syntax itself does not change.

- `cabal test media-page-test` runs the parser, golden and ffmpeg tests from
  the dev shell. After an intended rendering change, rerun it from this
  directory with `MEDIA_PAGE_UPDATE_GOLDEN=1` and review the golden diff.
- `checks.media-page-render` renders a page in Chromium the two ways the DSH
  sidebar previews HTML.
- Changing a ffmpeg preset's arguments requires a new preset identifier in
  `Workshop.MediaPage.Media`, so cached results from the old preset are not
  reused.
