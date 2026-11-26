{
  mkDerivation,
  base,
  lib,
}:
mkDerivation {
  pname = "workshop-test";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  description = "Assertions shared by Agent Workshop test suites";
  license = "unknown";
}
