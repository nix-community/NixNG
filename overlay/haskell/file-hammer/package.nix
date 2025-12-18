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
  hashable,
  lib,
  microlens,
  microlens-mtl,
  microlens-th,
  mtl,
  optparse-applicative,
  path,
  template-haskell,
  text,
  transformers,
  tree-diff,
  unix,
  unordered-containers,
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
    hashable
    microlens
    microlens-mtl
    microlens-th
    mtl
    optparse-applicative
    path
    template-haskell
    text
    transformers
    tree-diff
    unix
    unordered-containers
  ];
  license = lib.licenses.gpl3Plus;
  mainProgram = "file-hammer";
}
