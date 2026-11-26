{
  mkDerivation,
  aeson,
  base,
  base16-bytestring,
  blaze-html,
  bytestring,
  containers,
  cryptohash-sha256,
  directory,
  filepath,
  http-types,
  ihp-hsx,
  lib,
  process,
  scotty,
  stm,
  temporary,
  text,
  time,
  transformers,
  unix,
  wai,
  wai-extra,
  warp,
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
    blaze-html
    bytestring
    containers
    cryptohash-sha256
    directory
    filepath
    http-types
    ihp-hsx
    process
    scotty
    stm
    text
    time
    transformers
    unix
    wai
    wai-extra
    warp
    workshop-cli
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base
    bytestring
    directory
    filepath
    http-types
    process
    temporary
    text
    time
    wai
    wai-extra
    workshop-test
  ];
  description = "Record read-aloud books and publish them as private podcast feeds";
  license = "unknown";
  mainProgram = "abrec";
}
