{ mkDerivation
, aeson
, base
, bytestring
, lib
, unix
, unordered-containers
, vector
, cmdargs
}:
mkDerivation {
  pname = "sigell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    base
    bytestring
    unix
    unordered-containers
    vector
    cmdargs
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
