{
  lib,
  runCommand,
  fetchurl,
  wrapTool,
  ffmpeg-headless,
  curl,
  dejavu_fonts,
}:
let
  htmx = fetchurl {
    url = "https://unpkg.com/htmx.org@2.0.4/dist/htmx.min.js";
    hash = "sha256-4gndpcgjVHnzFm3vx3UOHbzVpcGAi3eS/C5nM3aPtEc=";
  };
  static = runCommand "abrec-static" { } ''
    mkdir -p "$out"
    cp ${./static/admin.css} "$out/admin.css"
    cp ${htmx} "$out/htmx.min.js"
  '';
in
wrapTool {
  name = "abrec";
  programs = {
    ABREC_FFMPEG = lib.getExe' ffmpeg-headless "ffmpeg";
    ABREC_FFPROBE = lib.getExe' ffmpeg-headless "ffprobe";
    ABREC_CURL = lib.getExe curl;
    ABREC_FONT = "${dejavu_fonts}/share/fonts/truetype/DejaVuSans-Bold.ttf";
    ABREC_STATIC = "${static}";
  };
}
