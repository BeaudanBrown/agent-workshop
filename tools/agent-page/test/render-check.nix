# Builds a media page and a question page with the packaged tool and renders
# them in Chromium the way the DSH sidebar previews HTML; see render-check.py.
{
  pkgs,
  ffmpeg,
  agent-page,
}:
let
  # The DOMPurify release DSH 0.2.2 bundles for its static HTML preview.
  dompurify = pkgs.fetchurl {
    url = "https://cdn.jsdelivr.net/npm/dompurify@3.4.13/dist/purify.min.js";
    hash = "sha256-mrPUTXPD45R/mrcuDwvBXH8ZMdYLNluiYfyF/lkBPFY=";
  };
  python = pkgs.python3.withPackages (ps: [ ps.playwright ]);
in
pkgs.runCommand "agent-page-render"
  {
    nativeBuildInputs = [
      python
      agent-page
    ];
    PLAYWRIGHT_BROWSERS_PATH = pkgs.playwright-driver.browsers;
    # The build sandbox has no fonts; without one, text lays out with no size.
    FONTCONFIG_FILE = pkgs.makeFontsConf { fontDirectories = [ pkgs.dejavu_fonts ]; };
  }
  ''
    export HOME=$TMPDIR AGENT_PAGE_OUT_DIR=$TMPDIR/pages
    mkdir media && cd media
    ${ffmpeg} -v error -f lavfi -i testsrc2=size=1280x720:rate=30:duration=2 -c:v libx264 clip.mp4
    ${ffmpeg} -v error -f lavfi -i testsrc2=size=800x450 -frames:v 1 still.png
    ${ffmpeg} -v error -f lavfi -i testsrc=size=160x120:rate=10:duration=0.8 anim.gif
    cat > page.md <<'EOF'
    ---
    title: Render check
    meta: Synthetic media only
    ---
    # Media
    A paragraph before the media.
    ![A synthetic clip](clip.mp4)
    # Tabs
    ::: tabs
    ## Still
    ![A synthetic still](still.png)
    ## Animation
    ![A synthetic animation](anim.gif)
    ## Text
    Plain text in the third tab.
    :::
    EOF
    cat > ask.md <<'EOF'
    ---
    title: Ask check
    reply: on
    ---
    Answer the questions below.

    ::: choice
    Which layout?

    ![A synthetic still](still.png)
    - Compact (recommended)
    - Spacious
    :::

    # Payroll
    ::: tabs
    ## Export
    ![Export still](still.png)
    ## Xero
    ::: multi
    Which checks?
    - Totals
    - Names
    :::
    :::

    ::: text
    Anything else?
    :::

    ::: choice
    Skip me?
    - Yes
    - No
    :::
    EOF
    python ${./render-check.py} "$(agent-page page.md)" "$(agent-page ask.md)" "$PWD/ask.md" ${dompurify}
    touch "$out"
  ''
