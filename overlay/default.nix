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
    src = final.fetchFromGitHub {
      owner = "blatt-linux";
      repo = "runit";
      rev = "f3843594034e8347a94595d891e5c74178962c7d";
      sha256 = "sha256-Ln5yuaxYCflZQnE58Gmm5WSfsmf+8+whyRIB3Pl8PCo=";
    };
    sourceRoot = "";

    doCheck = false;

    nativeBuildInputs = with prev; [ makeWrapper ];
    fixupPhase = ''
      wrapProgram $out/bin/sv \
        --set SVDIR "/service/"
    '';
  });

  inherit (callPackage ./trivial-builders.nix { })
    writeSubstitutedFile
    writeSubstitutedShellScript
    writeSubstitutedShellScriptBin
    ;

  # inherit
  #   (nixpkgsTrivialBuilders)
  #   writeShellScript
  #   writeShellScriptBin
  #   writeShellScriptApplication;
}
