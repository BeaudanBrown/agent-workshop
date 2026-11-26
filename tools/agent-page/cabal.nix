{
  mkDerivation,
  base,
  base16-bytestring,
  base64-bytestring,
  bytestring,
  cryptohash-sha256,
  directory,
  filepath,
  lib,
  process,
  temporary,
  text,
  unix,
  workshop-cli,
  workshop-test,
}:
mkDerivation {
  pname = "agent-page";
  version = "0.2.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    base16-bytestring
    base64-bytestring
    bytestring
    cryptohash-sha256
    directory
    filepath
    process
    text
    workshop-cli
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base
    bytestring
    directory
    filepath
    process
    temporary
    text
    unix
    workshop-test
  ];
  description = "Build self-contained HTML pages that present media and ask questions";
  license = "unknown";
  mainProgram = "agent-page";
}
