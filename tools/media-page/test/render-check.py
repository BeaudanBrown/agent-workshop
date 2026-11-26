"""Render a media-page in Chromium the two ways the DSH sidebar previews HTML.

static:   DOMPurify with the sidebar's settings, its CSP, and an iframe with no
          sandbox permissions (Coding Tools off).
scripted: the page unchanged in an iframe sandboxed to allow-scripts (Coding
          Tools on).

Both must play every video, show every image and switch tabs without scripts.
Usage: render-check.py PAGE.html purify.min.js
"""

import pathlib
import sys

from playwright.sync_api import sync_playwright

CSP = (
    "default-src 'none'; script-src 'none'; style-src 'unsafe-inline'; img-src data:; "
    "font-src data:; media-src data:; connect-src 'none'; frame-src 'none'; "
    "form-action 'none'; base-uri 'none'"
)
FORBID_TAGS = [
    "noscript", "base", "link", "meta", "iframe", "frame", "object", "embed",
    "set", "animate", "animateMotion", "animateTransform",
]

STATIC = """([source, csp, forbid]) => {
  const clean = DOMPurify.sanitize(source, {WHOLE_DOCUMENT: true, FORBID_TAGS: forbid, FORBID_ATTR: ['href', 'xlink:href']});
  const parsed = new DOMParser().parseFromString(clean, 'text/html');
  const policy = parsed.createElement('meta');
  policy.setAttribute('http-equiv', 'Content-Security-Policy');
  policy.setAttribute('content', csp);
  parsed.head.prepend(policy);
  const frame = document.getElementById('preview');
  frame.setAttribute('sandbox', '');
  frame.srcdoc = '<!doctype html>' + parsed.documentElement.outerHTML;
}"""

SCRIPTED = """(source) => {
  const frame = document.getElementById('preview');
  frame.setAttribute('sandbox', 'allow-scripts');
  frame.srcdoc = source;
}"""

LOADED = """() =>
  [...document.querySelectorAll('video')].every(v => v.readyState >= 1 || v.error) &&
  [...document.querySelectorAll('img')].every(i => i.complete)"""

STATE = """() => ({
  videos: [...document.querySelectorAll('video')].map(v => ({error: v.error && v.error.code, width: v.videoWidth})),
  images: [...document.querySelectorAll('img')].map(i => i.naturalWidth),
  captions: [...document.querySelectorAll('figcaption')].length,
})"""


def fail(mode, message):
    sys.exit(f"render-check ({mode}): {message}")


def check(browser, mode, html, purify):
    page = browser.new_page(viewport={"width": 1100, "height": 900})
    page.set_content('<iframe id="preview" style="width:1000px;height:1600px;border:0"></iframe>')
    if mode == "static":
        page.add_script_tag(content=purify)
        page.evaluate(STATIC, [html, CSP, FORBID_TAGS])
    else:
        page.evaluate(SCRIPTED, html)
    frame = page.wait_for_selector("#preview").content_frame()
    frame.wait_for_selector("h1", state="attached")
    frame.wait_for_function(LOADED, timeout=20000)
    state = frame.evaluate(STATE)
    if len(state["videos"]) != 2 or any(v["error"] or v["width"] == 0 for v in state["videos"]):
        fail(mode, f"videos did not load: {state['videos']}")
    if len(state["images"]) != 1 or 0 in state["images"]:
        fail(mode, f"images did not load: {state['images']}")
    if state["captions"] != 3:
        fail(mode, f"expected 3 captions, found {state['captions']}")
    panels = frame.locator(".tabs > .panel")
    if not (panels.nth(0).is_visible() and panels.nth(1).is_hidden()):
        fail(mode, "the first tab is not the only visible panel")
    frame.locator(".tabs > label").nth(1).click()
    if not (panels.nth(1).is_visible() and panels.nth(0).is_hidden()):
        fail(mode, "clicking the second tab did not switch panels")
    page.close()


def main():
    html = pathlib.Path(sys.argv[1]).read_text(encoding="utf-8")
    purify = pathlib.Path(sys.argv[2]).read_text(encoding="utf-8")
    with sync_playwright() as playwright:
        browser = playwright.chromium.launch()
        for mode in ("static", "scripted"):
            check(browser, mode, html, purify)
            print(f"render-check ({mode}): ok")
        browser.close()


main()
