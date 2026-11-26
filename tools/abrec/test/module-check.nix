# Evaluate nixosModules.abrec in a minimal system and check its wiring.
{
  self,
  nixpkgs,
  system,
}:
let
  inherit (nixpkgs) lib;
  pkgs = nixpkgs.legacyPackages.${system};
  evaluated = lib.nixosSystem {
    inherit system;
    modules = [
      self.nixosModules.abrec
      {
        boot.isContainer = true;
        system.stateVersion = "26.05";
        services.abrec = {
          enable = true;
          adminDomain = "abrec.example.com";
          publicDomain = "audiobooks.example.com";
        };
      }
    ];
  };
  inherit (evaluated) config;
  admin = config.services.nginx.virtualHosts."abrec.example.com";
  public = config.services.nginx.virtualHosts."audiobooks.example.com";
in
assert lib.assertMsg (
  admin.locations."/".proxyPass == "http://127.0.0.1:8790"
) "the admin site proxies to abrec";
assert lib.assertMsg (admin.locations."/".recommendedProxySettings) "the admin site forwards Host";
assert lib.assertMsg (
  public.root == "/var/lib/abrec/site"
) "the public site serves the current release";
assert lib.assertMsg (
  config.systemd.services.abrec.serviceConfig.User == "abrec"
) "abrec runs as its own user";
pkgs.runCommand "abrec-module" { } ''
  grep -q ' serve /nix/store/.*-abrec.json' ${config.systemd.units."abrec.service".unit}/abrec.service
  touch "$out"
''
