{
  mkDerivation,
  aeson,
  base,
  bytestring,
  bytestring-aeson-orphans,
  directory,
  exceptions,
  extra,
  filepath,
  Glob,
  hashable,
  lib,
  microlens,
  microlens-mtl,
  microlens-th,
  monad-logger,
  mtl,
  optparse-applicative,
  path,
  template-haskell,
  text,
  transformers,
  tree-diff,
  unix,
  unordered-containers,
  zlib,
}:
mkDerivation {
  pname = "file-hammer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    base
    bytestring
    bytestring-aeson-orphans
    directory
    exceptions
    extra
    filepath
    Glob
    hashable
    microlens
    microlens-mtl
    microlens-th
    monad-logger
    mtl
    optparse-applicative
    path
    template-haskell
    text
    transformers
    tree-diff
    unix
    unordered-containers
    zlib
  ];
  license = lib.licenses.gpl3Plus;
  mainProgram = "file-hammer";
}
