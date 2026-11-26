# Builds every workshop Haskell package and wraps tools for installation.
#
# Each package directory holds a cabal2nix-generated cabal.nix (kept current by
# checks.cabal-nix). A tool's package.nix calls wrapTool, which hands each
# runtime program to the tool through an environment variable.
{ pkgs, secretspec }:
let
  inherit (pkgs) lib;
  hlib = pkgs.haskell.lib.compose;

  packageDirs = {
    workshop-cli = ../lib/workshop-cli;
    workshop-test = ../lib/workshop-test;
    check-index = ../tools/check-index;
    agent-page = ../tools/agent-page;
    sec = ../tools/sec;
    abrec = ../tools/abrec;
  };

  ffmpeg = lib.getExe' pkgs.ffmpeg-headless "ffmpeg";
  ffprobe = lib.getExe' pkgs.ffmpeg-headless "ffprobe";
  # Title cards in abrec.
  abrecFont = "${pkgs.dejavu_fonts}/share/fonts/truetype/DejaVuSans-Bold.ttf";

  # Programs and arguments a test suite needs beyond its Haskell dependencies.
  testSetup = {
    agent-page = hlib.overrideCabal (drv: {
      preCheck = (drv.preCheck or "") + ''
        export AGENT_PAGE_FFMPEG=${ffmpeg} AGENT_PAGE_FFPROBE=${ffprobe}
      '';
    });
    abrec = hlib.overrideCabal (drv: {
      preCheck = (drv.preCheck or "") + ''
        export LC_ALL=C.UTF-8 ABREC_FFMPEG=${ffmpeg} ABREC_FFPROBE=${ffprobe} \
          ABREC_FONT=${abrecFont} ABREC_TEST_SHELL=${pkgs.runtimeShell}
      '';
    });
    sec = hlib.overrideCabal (drv: {
      testToolDepends = (drv.testToolDepends or [ ]) ++ [
        pkgs.git
        pkgs.util-linux
      ];
      preCheck = (drv.preCheck or "") + ''
        export LC_ALL=C.UTF-8 SEC_TEST_SHELL=${pkgs.runtimeShell}
      '';
      testFlags = [
        "dist/build/sec/sec"
        "${secretspec}/bin/secretspec"
      ];
    });
  };

  haskellPackages = pkgs.haskellPackages.override {
    overrides =
      hself: _:
      lib.mapAttrs (
        name: dir: (testSetup.${name} or lib.id) (hself.callPackage (dir + "/cabal.nix") { })
      ) packageDirs;
  };

  # Install a tool's executable with each runtime program set as VARIABLE=path.
  wrapTool =
    {
      name,
      programs ? { },
    }:
    let
      unwrapped = haskellPackages.${name};
    in
    pkgs.runCommand "${name}-${unwrapped.version}"
      {
        nativeBuildInputs = [ pkgs.makeWrapper ];
        meta.mainProgram = name;
      }
      ''
        install -Dm755 ${unwrapped}/bin/${name} "$out/libexec/${name}"
        makeWrapper "$out/libexec/${name}" "$out/bin/${name}" ${
          lib.concatStringsSep " " (
            lib.mapAttrsToList (variable: value: "--set ${variable} ${lib.escapeShellArg value}") programs
          )
        }
      '';
in
{
  inherit
    haskellPackages
    packageDirs
    wrapTool
    ffmpeg
    ffprobe
    abrecFont
    ;
}
