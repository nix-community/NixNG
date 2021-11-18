# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

self: super:
let
  inherit (super) haskell callPackage;
in
{
  tinyLinux = callPackage ./tiny-linux.nix { };
  runVmLinux = callPackage ./run-vm-linux.nix { };
  cronie = callPackage ./cronie.nix { };
  sigell = haskell.packages."ghc884".callPackage ./sigell/cabal.nix { };

  inherit (callPackage ./trivial-builders.nix {})
    writeSubstitutedFile
    writeSubstitutedShellScript
    writeSubstitutedShellScriptBin;
}
