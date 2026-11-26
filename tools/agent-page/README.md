# agent-page

Builds one self-contained HTML page from a short Markdown-style source file.
A page presents images and video, asks the reader questions, or both:

    agent-page notes.md

The command prints the page's path. `agent-page --help` documents the source
format with a complete example; errors are reported as `FILE:LINE`.

Pages that only present content work without scripts: media is embedded as
data URIs and tabs switch with CSS, so they render in the DSH sidebar's static
HTML preview as well as in a browser. Media is re-encoded with ffmpeg (H.264
MP4 video, WebP images) and cached beside the pages.

Text can include bulleted lists, code and diff blocks and CSV tables.

Pages with questions (`::: choice`, `::: multi`, `::: text`) or `reply: on`
carry one inline script. It collects what the reader enters, including
OK/Change/Question verdicts on media, into reply text that marks each answer
with its source line and fingerprints the source, and copies it to the
clipboard.
If the copy is blocked, it leaves the text selected for Ctrl/Cmd+C instead.
Nothing is stored, and previews that strip scripts show a notice to open the
page in a browser.

## Configuration

| Variable | Meaning | Default |
| --- | --- | --- |
| `AGENT_PAGE_OUT_DIR` | Directory for pages and the encoding cache | `/tmp/agent-page` |
| `AGENT_PAGE_MAX_BYTES` | Largest page written, in bytes | `67108864` (64 MiB) |
| `AGENT_PAGE_FFMPEG`, `AGENT_PAGE_FFPROBE` | Set by the Nix wrapper | — |

`-o PATH` writes one page elsewhere; the cache stays in `AGENT_PAGE_OUT_DIR`.
Keep the size cap at or below the DSH `workspace-files.maxFileBytes` setting,
or the sidebar refuses pages the tool accepted.

## Development

The parser accepts only the documented grammar. A new feature is a new
directive name in `Workshop.AgentPage.Parse.containerDirectives` plus its
rendering in `Workshop.AgentPage.Render`; the syntax itself does not change.
The reply text format is produced by the script in `Workshop.AgentPage.Render`
and announced as `format 3`. Change the format version when it changes, and
update the skill's description of replies.

- `cabal test agent-page-test` runs the parser, golden and ffmpeg tests from
  the dev shell. After an intended rendering change, rerun it from this
  directory with `AGENT_PAGE_UPDATE_GOLDEN=1` and review the golden diff.
- `checks.agent-page-render` renders a media page and a question page in
  Chromium the two ways the DSH sidebar previews HTML. On the question page it
  fills in every kind of answer, presses Copy reply and compares the reply
  text exactly.
- Changing a ffmpeg preset's arguments requires a new preset identifier in
  `Workshop.AgentPage.Media`, so cached results from the old preset are not
  reused.
