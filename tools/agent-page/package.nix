{
  lib,
  wrapTool,
  ffmpeg-headless,
}:
wrapTool {
  name = "agent-page";
  programs = {
    AGENT_PAGE_FFMPEG = lib.getExe' ffmpeg-headless "ffmpeg";
    AGENT_PAGE_FFPROBE = lib.getExe' ffmpeg-headless "ffprobe";
  };
}
