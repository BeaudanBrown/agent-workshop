{
  mkDerivation,
  base,
  filepath,
  lib,
}:
mkDerivation {
  pname = "workshop-cli";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    base
    filepath
  ];
  description = "Runtime helpers shared by Agent Workshop tools";
  license = "unknown";
}
