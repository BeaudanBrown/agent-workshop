{
  mkDerivation,
  aeson,
  base,
  base16-bytestring,
  bytestring,
  cryptohash-sha256,
  directory,
  filepath,
  lib,
  temporary,
  text,
  time,
  unix,
  workshop-test,
}:
mkDerivation {
  pname = "abrec";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    base16-bytestring
    bytestring
    cryptohash-sha256
    directory
    filepath
    text
    time
    unix
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base
    directory
    filepath
    temporary
    text
    time
    workshop-test
  ];
  description = "Record read-aloud books and publish them as private podcast feeds";
  license = "unknown";
  mainProgram = "abrec";
}
