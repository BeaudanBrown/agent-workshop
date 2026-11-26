{
  lib,
  wrapTool,
  ffmpeg-headless,
  curl,
  dejavu_fonts,
}:
wrapTool {
  name = "abrec";
  programs = {
    ABREC_FFMPEG = lib.getExe' ffmpeg-headless "ffmpeg";
    ABREC_FFPROBE = lib.getExe' ffmpeg-headless "ffprobe";
    ABREC_CURL = lib.getExe curl;
    ABREC_FONT = "${dejavu_fonts}/share/fonts/truetype/DejaVuSans-Bold.ttf";
  };
}
