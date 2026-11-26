{
  mkDerivation,
  aeson,
  base,
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
  pname = "sec";
  version = "0.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson
    base
    directory
    filepath
    process
    text
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
    workshop-test
  ];
  description = "Run a project command with SecretSpec values from Bitwarden";
  license = "unknown";
  mainProgram = "sec";
}
