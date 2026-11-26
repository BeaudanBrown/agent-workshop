---
name: agent-page
description: Show the user screenshots or video, or ask them a batch of questions, as one self-contained HTML page they answer and copy back. Use when a reader must see a result (UI changes, demo recordings, visual reviews) or when several questions, such as a round of interview or grilling questions, are easier to answer together than one at a time in chat.
---

`agent-page` turns a short source file, plus any media, into one HTML page that opens in the DSH sidebar and any browser. The page can present media, ask questions, or both. Its `--help` is the format reference; run it before writing a source file.

Choose the channel first. One or two questions, especially when each depends on the previous answer, go through the harness's own question tool or chat. A page suits a batch of independent questions, questions that need screenshots beside them, and reviews where the user comments on each item.

1. Collect any media. Recording and screenshots come from the project's own tooling. Keep each video to the moment that matters, and a page's video to about one to three minutes in total.
2. Write the source file in the directory that holds the media, so its paths stay relative.
   - Give every image and video a caption that states what the reader sees and why it matters; caption only what you verified.
   - Ask with `::: choice`, `::: multi` or `::: text`. Put context the user needs to decide right under the question, and mark at most one option recommended.
   - Add `reply: on` to the header when you want feedback on each image, video and section: images and video get OK, Change and Question buttons and a comment box.
   - Use `- item` lists, code and diff blocks and ````csv` tables for text the user must read precisely, never screenshots of text.
3. Run `agent-page FILE`. Each error names `FILE:LINE`; fix the source and rerun until the command prints a path.
4. Present the printed path to the user (`present` in DSH), with a short summary of what the page shows or asks. Tell them to press Copy reply and paste the result into the chat.

A pasted reply starts with `agent-page reply`. Its header gives the source path and the first 8 hex digits of the source's SHA-256 when the page was built. If `sha256sum` of the source now differs, the line numbers may have moved: find each entry by its quoted question or caption instead. Each entry names the source `FILE:LINE` and quotes the question or caption it answers, then gives the answer, verdict or comment. Skipped questions are listed at the end. Act on the answers. If you need another round, build a new page rather than editing the answered one.
