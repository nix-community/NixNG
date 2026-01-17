{
  mkDerivation,
  aeson,
  async,
  base,
  brick,
  bytestring,
  bytestring-aeson-orphans,
  conduit,
  effectful,
  extra,
  lib,
  megaparsec,
  microlens-platform,
  monad-logger,
  mtl,
  nixng,
  optparse-applicative,
  path,
  require-callstack,
  text,
  time,
  transformers,
  typed-process,
  typed-process-effectful,
  unix,
  unordered-containers,
  vty,
  vty-crossplatform,
}:
mkDerivation {
  pname = "mrsk";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    async
    base
    brick
    bytestring
    bytestring-aeson-orphans
    conduit
    effectful
    extra
    megaparsec
    microlens-platform
    monad-logger
    mtl
    nixng
    optparse-applicative
    path
    require-callstack
    text
    time
    transformers
    typed-process
    typed-process-effectful
    unix
    unordered-containers
    vty
    vty-crossplatform
  ];
  homepage = "https://github.com/MagicRB/NixNG/tree/feat/environment/overlay/haskell/mrsk";
  description = "Nix* Deployments";
  license = lib.licenses.gpl3Plus;
  mainProgram = "mrsk";
}
