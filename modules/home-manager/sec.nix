{ self, sec-tools-nixpkgs }:
{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.programs.sec;
  system = pkgs.stdenv.hostPlatform.system;
in
{
  options.programs.sec = {
    enable = lib.mkEnableOption "the interactive Agent Workshop SecretSpec launcher";
    vaultwardenUrl = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = null;
      example = "https://vault.example.com";
      description = "Expected HTTPS Vaultwarden server. Null uses hosted Bitwarden.";
    };
    bwPackage = lib.mkPackageOption pkgs "bitwarden-cli" { };
    secretspecPackage = lib.mkPackageOption sec-tools-nixpkgs.legacyPackages.${system} "secretspec" { };
  };
  config = lib.mkIf cfg.enable {
    home.packages = [
      (self.packages.${system}.sec.override {
        bw = cfg.bwPackage;
        secretspec = cfg.secretspecPackage;
        inherit (cfg) vaultwardenUrl;
      })
    ];
  };
}
