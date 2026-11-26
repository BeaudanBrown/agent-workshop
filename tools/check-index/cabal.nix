{
  mkDerivation,
  base,
  filepath,
  lib,
  process,
  temporary,
}:
mkDerivation {
  pname = "check-index";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    filepath
    process
    temporary
  ];
  executableHaskellDepends = [ base ];
  description = "Validate the staged Git snapshot with nix flake check";
  license = "unknown";
  mainProgram = "check-index";
}
