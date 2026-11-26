{
  lib,
  wrapTool,
  ffmpeg-headless,
}:
wrapTool {
  name = "media-page";
  programs = {
    MEDIA_PAGE_FFMPEG = lib.getExe' ffmpeg-headless "ffmpeg";
    MEDIA_PAGE_FFPROBE = lib.getExe' ffmpeg-headless "ffprobe";
  };
}
