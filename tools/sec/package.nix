{
  lib,
  wrapTool,
  bw,
  secretspec,
  vaultwardenUrl ? null,
}:
wrapTool {
  name = "sec";
  programs = {
    SEC_BW = lib.getExe' bw "bw";
    SEC_SECRETSPEC = lib.getExe' secretspec "secretspec";
  }
  // lib.optionalAttrs (vaultwardenUrl != null) { SEC_VAULTWARDEN_URL = vaultwardenUrl; };
}
