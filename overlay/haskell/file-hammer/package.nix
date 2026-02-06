# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

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
    microlens_0_5_0_0
    (microlens-mtl_0_2_1_1.override { microlens = microlens_0_5_0_0; })
    (microlens-th_0_4_3_18.override { microlens = microlens_0_5_0_0; })
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

  configureFlags = [
    "--ghc-option=-Werror"
  ];

  license = lib.licenses.gpl3Plus;
  mainProgram = "file-hammer";
}
