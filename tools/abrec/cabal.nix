{
  mkDerivation,
  aeson,
  base,
  base16-bytestring,
  bytestring,
  containers,
  cryptohash-sha256,
  directory,
  filepath,
  lib,
  process,
  temporary,
  text,
  time,
  unix,
  workshop-cli,
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
    containers
    cryptohash-sha256
    directory
    filepath
    process
    text
    time
    unix
    workshop-cli
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base
    directory
    filepath
    process
    temporary
    text
    time
    workshop-test
  ];
  description = "Record read-aloud books and publish them as private podcast feeds";
  license = "unknown";
  mainProgram = "abrec";
}
