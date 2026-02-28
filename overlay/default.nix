# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

final: prev:
let
  inherit (final) haskellPackages;
  inherit (prev) callPackage;
in
{
  tinyLinux = callPackage ./tiny-linux.nix { };
  runVmLinux = final.callPackage ./run-vm-linux.nix { };
  cronie = callPackage ./cronie.nix { };
  pause = callPackage ./pause.nix { };
  sigell = haskellPackages.callPackage ./sigell/cabal.nix { };
  systemdStandalone = callPackage ./systemd-minimal.nix { };
  systemdTmpfilesD = callPackage ./systemd-tmpfiles.d.nix { };
  dinit = callPackage ./dinit.nix { };

  util-linuxSystemdFree = prev.util-linux.override {
    systemdSupport = false;
    pamSupport = true;
  };
  runit = prev.runit.overrideAttrs (old: {
    nativeBuildInputs = old.nativeBuildInputs or [ ] ++ (with prev; [ makeWrapper ]);
    fixupPhase = old.fixupPhase or "" + ''
      wrapProgram $out/bin/sv \
        --set SVDIR "/service/"
    '';
  });

  inherit (callPackage ./trivial-builders.nix { })
    writeSubstitutedFile
    writeSubstitutedShellScript
    writeSubstitutedShellScriptBin
    ;

  setgroups = prev.writeCBin "setgroups" (builtins.readFile ./setgroups.c);

  nixng = prev.haskellPackages.callPackage ./haskell/nixng/package.nix { };

  fileHammer = prev.haskell.lib.enableSeparateBinOutput (
    prev.haskellPackages.callPackage ./haskell/file-hammer/package.nix { }
  );

  mrsk =
    let
      hpkgs = final.haskellPackages;
      microlens = hpkgs.microlens_0_5_0_0;
      microlens-ghc = hpkgs.microlens-ghc_0_4_15_2.override { inherit microlens; };
      microlens-th = hpkgs.microlens-th_0_4_3_18.override { inherit microlens; };
      microlens-mtl = hpkgs.microlens-mtl_0_2_1_1.override { inherit microlens; };
      microlens-platform = hpkgs.microlens-platform_0_4_4_2.override {
        inherit
          microlens
          microlens-ghc
          microlens-th
          microlens-mtl
          ;
      };
      vty = hpkgs.vty.override {
        inherit microlens microlens-mtl;
      };
      vty-unix = hpkgs.vty-unix.override {
        inherit
          microlens
          microlens-mtl
          microlens-th
          vty
          ;
      };
      vty-crossplatform = hpkgs.vty-crossplatform.override {
        inherit vty vty-unix;
      };
      brick = hpkgs.brick_2_10.override {
        inherit
          microlens
          microlens-th
          microlens-mtl
          vty
          vty-crossplatform
          ;
      };
    in
    prev.haskell.lib.enableSeparateBinOutput (
      prev.haskellPackages.callPackage ./haskell/mrsk/package.nix {
        inherit (final) nixng;
        typed-process-effectful = final.lib.pipe final.haskellPackages.typed-process-effectful [
          final.haskell.lib.markUnbroken
          final.haskell.lib.dontCheck
        ];

        inherit
          brick
          microlens-platform
          vty
          vty-crossplatform
          ;
      }
    );

  # inherit
  #   (nixpkgsTrivialBuilders)
  #   writeShellScript
  #   writeShellScriptBin
  #   writeShellScriptApplication;
}
