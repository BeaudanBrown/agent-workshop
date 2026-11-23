{
  lib,
  stdenv,
  ghc,
  makeWrapper,
  bw,
  secretspec,
  vaultwardenUrl ? null,
}:
stdenv.mkDerivation {
  pname = "sec";
  version = "0.1.0";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = ./Main.hs;
  };
  nativeBuildInputs = [
    (ghc.withPackages (p: [ p.aeson ]))
    makeWrapper
  ];
  buildPhase = ''
    runHook preBuild
    ghc -Wall -Werror -O2 Main.hs -o sec
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    install -Dm755 sec "$out/libexec/sec"
    makeWrapper "$out/libexec/sec" "$out/bin/sec" \
      --set SEC_BW ${lib.getExe' bw "bw"} \
      --set SEC_SECRETSPEC ${lib.getExe' secretspec "secretspec"} ${
        lib.optionalString (
          vaultwardenUrl != null
        ) "--set SEC_VAULTWARDEN_URL ${lib.escapeShellArg vaultwardenUrl}"
      }
    runHook postInstall
  '';
  meta.mainProgram = "sec";
}
