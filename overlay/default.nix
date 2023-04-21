# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

final: prev:
let
  inherit (prev) haskellPackages callPackage;
  nixpkgsTrivialBuilders =
    final.callPackage "${prev.path}/pkgs/build-support/trivial-builders.nix" {
      runtimeShell = final.busybox + "/bin/sh";
    };
in
{
  tinyLinux = callPackage ./tiny-linux.nix { };
  runVmLinux = final.callPackage ./run-vm-linux.nix { };
  cronie = callPackage ./cronie.nix { };
  sigell = haskellPackages.callPackage ./sigell/cabal.nix { };
  util-linuxSystemdFree = prev.util-linux.override {
    systemdSupport = false;
    pamSupport = true;
  };
  runit = prev.runit.overrideAttrs
    (old:
      {
        src = final.fetchFromGitHub {
          owner = "blatt-linux";
          repo = "runit";
          rev = "19edc11741498fc7aafd5c7175f56e41f7c945f9";
          sha256 = "sha256-yx8rVubWkA2jQS2d3IuarZwJq4pQ+gmIYzv7tVMg7zA=";
        };
        sourceRoot = "";
      });

  inherit (callPackage ./trivial-builders.nix {})
    writeSubstitutedFile
    writeSubstitutedShellScript
    writeSubstitutedShellScriptBin;

  # inherit
  #   (nixpkgsTrivialBuilders)
  #   writeShellScript
  #   writeShellScriptBin
  #   writeShellScriptApplication;
}
