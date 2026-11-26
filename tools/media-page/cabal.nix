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
  pname = "media-page";
  version = "0.1.0";
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
  description = "Build self-contained HTML pages that present images and video";
  license = "unknown";
  mainProgram = "media-page";
}
