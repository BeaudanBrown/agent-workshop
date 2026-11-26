"""Render agent-pages in Chromium the two ways the DSH sidebar previews HTML.

static:   DOMPurify with the sidebar's settings, its CSP, and an iframe with no
          sandbox permissions (Coding Tools off).
scripted: the page unchanged, loaded from a blob URL into an iframe sandboxed
          to allow-scripts and nothing else (Coding Tools on). The host page is
          served from an https origin, as DSH is.

The media page must play every video, show every image, switch tabs and show
its list, code, diff and table in both modes. The question page must show its no-scripts notice in static mode,
and in scripted mode turn what is entered, including verdicts, into the exact
reply text.
Usage: render-check.py MEDIA.html QUESTIONS.html QUESTIONS-SOURCE purify.min.js
"""

import hashlib
import pathlib
import sys

from playwright.sync_api import sync_playwright

HOST = "https://dsh.test/"
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
  frame.src = URL.createObjectURL(new Blob([source], {type: 'text/html'}));
}"""

LOADED = """() =>
  [...document.querySelectorAll('video')].every(v => v.readyState >= 1 || v.error) &&
  [...document.querySelectorAll('img')].every(i => i.complete)"""

STATE = """() => ({
  videos: [...document.querySelectorAll('video')].map(v => ({error: v.error && v.error.code, width: v.videoWidth})),
  images: [...document.querySelectorAll('img')].map(i => i.naturalWidth),
  captions: [...document.querySelectorAll('figcaption')].length,
  scripts: document.querySelectorAll('script').length,
  items: document.querySelectorAll('ul > li').length,
  added: document.querySelectorAll('.code .add').length,
  removed: document.querySelectorAll('.code .del').length,
  codeBlocks: document.querySelectorAll('.code pre').length,
  cells: document.querySelectorAll('.table td').length,
})"""

COPIED = "Copied. Paste it into the chat."
BLOCKED = "This view blocked copying. The text below is selected: press Ctrl+C or ⌘C."


def fail(mode, message):
    sys.exit(f"render-check ({mode}): {message}")


def open_preview(browser, mode, html, purify):
    page = browser.new_page(viewport={"width": 1100, "height": 900})
    page.route(HOST, lambda route: route.fulfill(
        content_type="text/html",
        body='<!doctype html><iframe id="preview" style="width:1000px;height:1600px;border:0"></iframe>',
    ))
    page.goto(HOST)
    if mode == "static":
        page.add_script_tag(content=purify)
        page.evaluate(STATIC, [html, CSP, FORBID_TAGS])
    else:
        page.evaluate(SCRIPTED, html)
    frame = page.wait_for_selector("#preview").content_frame()
    frame.wait_for_selector("h1", state="attached")
    frame.wait_for_function(LOADED, timeout=20000)
    return page, frame


def check_media(browser, mode, html, purify):
    page, frame = open_preview(browser, mode, html, purify)
    state = frame.evaluate(STATE)
    if len(state["videos"]) != 2 or any(v["error"] or v["width"] == 0 for v in state["videos"]):
        fail(mode, f"videos did not load: {state['videos']}")
    if len(state["images"]) != 1 or 0 in state["images"]:
        fail(mode, f"images did not load: {state['images']}")
    if state["captions"] != 3:
        fail(mode, f"expected 3 captions, found {state['captions']}")
    if state["scripts"] != 0:
        fail(mode, "a page without questions or replies carries a script")
    blocks = {key: state[key] for key in ("items", "added", "removed", "codeBlocks", "cells")}
    if blocks != {"items": 2, "added": 1, "removed": 1, "codeBlocks": 2, "cells": 4}:
        fail(mode, f"lists, code or tables did not render as expected: {blocks}")
    panels = frame.locator(".tabs > .panel")
    if not (panels.nth(0).is_visible() and panels.nth(1).is_hidden()):
        fail(mode, "the first tab is not the only visible panel")
    frame.locator(".tabs > label").nth(1).click()
    if not (panels.nth(1).is_visible() and panels.nth(0).is_hidden()):
        fail(mode, "clicking the second tab did not switch panels")
    page.close()


def check_questions_static(browser, html, purify):
    mode = "static questions"
    page, frame = open_preview(browser, "static", html, purify)
    if not frame.locator(".ap-static").is_visible():
        fail(mode, "the no-scripts notice is hidden")
    if frame.locator(".ap-live").is_visible():
        fail(mode, "the copy controls show although scripts cannot run")
    if frame.locator("fieldset.q").count() != 4:
        fail(mode, "the questions did not survive sanitising")
    page.close()


def check_questions_scripted(browser, html, source):
    mode = "scripted questions"
    page, frame = open_preview(browser, "scripted", html, "")
    if frame.locator(".ap-static").is_visible() or not frame.locator(".ap-live").is_visible():
        fail(mode, "the script did not switch the notice for the copy controls")
    q = frame.locator("fieldset.q")
    q.nth(0).get_by_label("Spacious").check()
    q.nth(0).locator("summary").click()
    q.nth(0).locator(".ap-note").fill("Looks cramped")
    if frame.locator("fieldset.q").nth(0).locator(".table td").count() != 2:
        fail(mode, "the table inside the first question did not render")
    export = frame.locator("figure", has_text="Export still")
    export.locator(".verdict label", has_text="Change").click()
    export.locator("summary").click()
    export.locator("textarea").fill("Crop tighter")
    frame.locator("figure", has_text="Second still").locator(".verdict label", has_text="OK").click()
    frame.locator(".tabs > label", has_text="Xero").click()
    q.nth(1).get_by_label("Totals").check()
    q.nth(1).locator(".ap-other").fill("Dates")
    q.nth(2).locator(".ap-answer").fill("Ship it\nsoon")
    section = frame.locator("section > details.reply")
    section.locator("summary").click()
    section.locator("textarea").fill("Good section")
    frame.locator("#ap-overall").fill("Thanks")
    frame.locator("#ap-copy").click()
    frame.wait_for_function(f"() => [{COPIED!r}, {BLOCKED!r}].includes(document.getElementById('ap-status').textContent)")
    file = pathlib.Path(source).name
    digest = hashlib.sha256(pathlib.Path(source).read_bytes()).hexdigest()[:8]
    expected = "\n".join([
        "agent-page reply · format 3",
        "page: Ask check",
        f"source: {source} · sha256 {digest}",
        "",
        f"Q1 · {file}:8",
        "> Which layout?",
        "→ Spacious",
        "note: Looks cramped",
        "",
        f"Reply · {file}:23 · Payroll › Export · image",
        '> "Export still"',
        "→ Change",
        "Crop tighter",
        "",
        f"Reply · {file}:24 · Payroll › Export · image",
        '> "Second still"',
        "→ OK",
        "",
        f"Q2 · {file}:27 · Payroll › Xero",
        "> Which checks?",
        "→ Totals; Other: Dates",
        "",
        f"Q3 · {file}:34 · Payroll",
        "> Anything else?",
        "Ship it",
        "soon",
        "",
        f"Reply · {file}:20 · Payroll · section",
        "Good section",
        "",
        "Overall",
        "Thanks",
        "",
        "Skipped: Q4",
        "",
    ])
    actual = frame.locator("#ap-out").input_value()
    if actual != expected:
        fail(mode, f"unexpected reply text:\n{actual}\n--- expected ---\n{expected}")
    if not frame.locator("fieldset.q").nth(1).locator(".ap-other-pick").is_checked():
        fail(mode, "typing an Other answer did not select Other")
    selected = frame.evaluate("() => { const o = document.getElementById('ap-out'); return [o.selectionStart, o.selectionEnd, o.value.length]; }")
    if selected[0] != 0 or selected[1] != selected[2]:
        fail(mode, f"the reply text is not selected for manual copying: {selected}")
    status = frame.locator("#ap-status").text_content()
    print(f"render-check ({mode}): copy status: {status}")
    page.close()


def main():
    media = pathlib.Path(sys.argv[1]).read_text(encoding="utf-8")
    questions = pathlib.Path(sys.argv[2]).read_text(encoding="utf-8")
    source = sys.argv[3]
    purify = pathlib.Path(sys.argv[4]).read_text(encoding="utf-8")
    with sync_playwright() as playwright:
        browser = playwright.chromium.launch()
        for mode in ("static", "scripted"):
            check_media(browser, mode, media, purify)
            print(f"render-check ({mode}): ok")
        check_questions_static(browser, questions, purify)
        print("render-check (static questions): ok")
        check_questions_scripted(browser, questions, source)
        print("render-check (scripted questions): ok")
        browser.close()


main()
