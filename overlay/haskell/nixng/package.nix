{
  mkDerivation,
  aeson,
  base,
  bytestring,
  exceptions,
  hashable,
  lib,
  microlens_0_5_0_0,
  microlens-th_0_4_3_18,
  mtl,
  optparse-applicative,
  path,
  template-haskell,
  text,
  typed-process,
  unix,
  unliftio,
}:
mkDerivation {
  pname = "nixng";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    exceptions
    hashable
    microlens_0_5_0_0
    (microlens-th_0_4_3_18.override { microlens = microlens_0_5_0_0; })
    mtl
    optparse-applicative
    path
    template-haskell
    text
    typed-process
    unix
    unliftio
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/MagicRB/NixNG/tree/feat/environment/overlay/haskell/nixng";
  license = lib.licenses.mpl20;
}
