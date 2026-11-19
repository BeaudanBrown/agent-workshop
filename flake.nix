{
  description = "Agent Workshop Haskell development environment and curated skills";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
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
