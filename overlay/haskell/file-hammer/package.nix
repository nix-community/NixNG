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
  microlens_0_5_0_0,
  microlens-mtl_0_2_1_1,
  microlens-th_0_4_3_18,
  monad-logger,
  mtl,
  nixng,
  optparse-applicative,
  path,
  pretty,
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
    Glob
    hashable
    microlens_0_5_0_0
    (microlens-mtl_0_2_1_1.override { microlens = microlens_0_5_0_0; })
    (microlens-th_0_4_3_18.override { microlens = microlens_0_5_0_0; })
    monad-logger
    mtl
    nixng
    optparse-applicative
    path
    pretty
    text
    transformers
    tree-diff
    unix
    unordered-containers
  ];

  license = lib.licenses.gpl3Plus;
  mainProgram = "file-hammer";
}
