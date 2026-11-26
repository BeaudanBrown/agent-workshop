{
  description = "Agent Workshop Haskell tools, curated skills and Home Manager modules";

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
      inherit (nixpkgs) lib;
      forEachSystem = lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
      ];
      formatting = forEachSystem (
        system: treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} ./treefmt.nix
      );
      workshop = forEachSystem (
        system:
        import ./nix/workshop.nix {
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (sec-tools-nixpkgs.legacyPackages.${system}) secretspec;
        }
      );
      upstreamSkills = {
        domain-modeling = "${matt-pocock-skills}/skills/engineering/domain-modeling";
        grill-with-docs = "${matt-pocock-skills}/skills/engineering/grill-with-docs";
        grilling = "${matt-pocock-skills}/skills/productivity/grilling";
        writing-for-agents = "${matt-pocock-skills}/skills/productivity/writing-for-agents";
      };
      selectedSkills = upstreamSkills;
    in
    {
      homeManagerModules.sec = import ./modules/home-manager/sec.nix { inherit self sec-tools-nixpkgs; };

      packages = forEachSystem (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          inherit (workshop.${system}) wrapTool;
        in
        {
          check-index = pkgs.callPackage ./tools/check-index/package.nix { inherit wrapTool; };
          media-page = pkgs.callPackage ./tools/media-page/package.nix { inherit wrapTool; };
          sec = pkgs.callPackage ./tools/sec/package.nix {
            inherit wrapTool;
            bw = pkgs.bitwarden-cli;
            inherit (sec-tools-nixpkgs.legacyPackages.${system}) secretspec;
          };
          skills = pkgs.linkFarm "agent-workshop-skills" (
            lib.mapAttrsToList (
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
          inherit (workshop.${system}) packageDirs ffmpeg;
        in
        {
          formatting = formatting.${system}.config.build.check self;
          # Building a tool runs its test suite.
          inherit (packages) check-index media-page sec;
          cabal-nix =
            pkgs.runCommand "cabal-nix-current"
              {
                nativeBuildInputs = [
                  pkgs.cabal2nix
                  pkgs.nixfmt
                ];
              }
              ''
                export HOME=$TMPDIR
                ${lib.concatStrings (
                  lib.mapAttrsToList (name: dir: ''
                    cp -r ${dir} package && chmod -R u+w package
                    (cd package && cabal2nix . | nixfmt - > ../generated.nix)
                    diff -u ${dir}/cabal.nix generated.nix || {
                      echo "${name}: regenerate cabal.nix with: cabal2nix . | nixfmt - > cabal.nix"
                      exit 1
                    }
                    rm -rf package generated.nix
                  '') packageDirs
                )}
                touch "$out"
              '';
          hlint = pkgs.runCommand "hlint" { nativeBuildInputs = [ pkgs.hlint ]; } ''
            cd ${./.}
            hlint lib tools
            touch "$out"
          '';
          media-page-render = import ./tools/media-page/test/render-check.nix {
            inherit pkgs ffmpeg;
            media-page = packages.media-page;
          };
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
          inherit (workshop.${system})
            haskellPackages
            packageDirs
            ffmpeg
            ffprobe
            ;
        in
        {
          default = haskellPackages.shellFor {
            packages = hp: map (name: hp.${name}) (lib.attrNames packageDirs);
            nativeBuildInputs = [
              pkgs.cabal-install
              pkgs.cabal2nix
              pkgs.hlint
              pkgs.nixfmt
            ];
            MEDIA_PAGE_FFMPEG = ffmpeg;
            MEDIA_PAGE_FFPROBE = ffprobe;
          };
        }
      );
    };
}
