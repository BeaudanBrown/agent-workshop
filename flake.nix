{
  description = "Agent Workshop Haskell development environment and curated skills";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    # This pin provides SecretSpec 0.21.1 with the bw provider.
    sec-tools-nixpkgs.url = "github:NixOS/nixpkgs/623ef1c827745a0d653f72992ac1e222e81ebb2a";
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    matt-pocock-skills = {
      url = "github:mattpocock/skills";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      matt-pocock-skills,
      treefmt-nix,
      sec-tools-nixpkgs,
      ...
    }:
    let
      forEachSystem = nixpkgs.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
      ];
      formatting = forEachSystem (
        system: treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} ./treefmt.nix
      );
      selectedSkills = {
        writing-for-agents = "${matt-pocock-skills}/skills/productivity/writing-for-agents";
      };
    in
    {
      homeManagerModules.sec = import ./modules/home-manager/sec.nix { inherit self sec-tools-nixpkgs; };

      packages = forEachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          check-index = pkgs.stdenv.mkDerivation {
            pname = "check-index";
            version = "0.1";
            src = ./tools/check-index;
            nativeBuildInputs = [ (pkgs.ghc.withPackages (p: [ p.temporary ])) ];
            buildPhase = "ghc -Wall -Werror Main.hs -o check-index";
            installPhase = "install -Dm755 check-index $out/bin/check-index";
          };
          sec = pkgs.callPackage ./tools/sec/package.nix {
            bw = pkgs.bitwarden-cli;
            inherit (sec-tools-nixpkgs.legacyPackages.${system}) secretspec;
          };
          skills = pkgs.linkFarm "agent-workshop-skills" (
            nixpkgs.lib.mapAttrsToList (
              name: path:
              if builtins.pathExists "${path}/SKILL.md" then
                { inherit name path; }
              else
                throw "Selected skill '${name}' is missing SKILL.md in ${path}"
            ) selectedSkills
          );
        }
      );

      formatter = forEachSystem (system: formatting.${system}.config.build.wrapper);
      checks = forEachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          packages = self.packages.${system};
        in
        {
          formatting = formatting.${system}.config.build.check self;
          inherit (packages) check-index;
          sec =
            pkgs.runCommand "sec-tests"
              {
                nativeBuildInputs = [
                  (pkgs.ghc.withPackages (p: [ p.temporary ]))
                  pkgs.git
                  pkgs.util-linux
                ];
              }
              ''
                cp ${./tools/sec/test/Main.hs} Main.hs
                ghc -Wall -Werror Main.hs -o sec-tests
                SEC_TEST_SHELL=${pkgs.runtimeShell} ./sec-tests ${packages.sec}/libexec/sec ${
                  sec-tools-nixpkgs.legacyPackages.${system}.secretspec
                }/bin/secretspec
                touch "$out"
              '';
          writing-for-agents-reference = pkgs.runCommand "writing-for-agents-reference" { } ''
            test -r ${packages.skills}/writing-for-agents/SKILL-MECHANICS.md
            touch "$out"
          '';
        }
      );
      devShells = forEachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
        in
        {
          default = pkgs.mkShell {
            packages = [
              (pkgs.ghc.withPackages (p: [ p.temporary ]))
              pkgs.cabal-install
              pkgs.hlint
            ];
          };
        }
      );
    };
}
