# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

# TODO use linuxManualConfig instead of buildLinux

{
  buildLinux,
  linux,
  pkg-config,
  ncurses,
  writeText,
  lib,
  extraConfig ? { },
}:
let
  configfile = writeText "config" ''
    ${lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: "${n}=${v}") extraConfig)}
  '';
  origKernel = buildLinux {
    inherit (linux) src version stdenv;
    inherit configfile;

    kernelPatches = [ ];
    allowImportFromDerivation = true;
  };
  self = origKernel.overrideAttrs (old: {
    buildFlags = [
      "KBUILD_BUILD_VERSION=1-NixNG"
      "bzImage"
      "vmlinux"
    ];
    configurePhase = ''
      runHook preConfigure

      mkdir build
      export buildRoot="$(pwd)/build"

      echo "manual-config configurePhase buildRoot=$buildRoot pwd=$PWD"

      runHook postConfigure

      # make $makeFlags "''${makeFlagsArray[@]}" mrproper

      make $makeFlags "''${makeFlagsArray[@]}" tinyconfig
      if [[ -f $buildRoot/.config ]] ; then
        cat ${configfile} >> $buildRoot/.config
        cat $buildRoot/.config
      else
        echo "$buildRoot/.config is empty, not appending"
        exit 1
      fi
      echo $buildFlags

      # make $makeFlags "''${makeFlagsArray[@]}" prepare

      # Note: https://github.com/NixOS/nixpkgs/blob/9c213398b312e0f0bb9cdf05090fd20223a82ad0/pkgs/os-specific/linux/kernel/manual-config.nix#L166
      buildFlagsArray+=("KBUILD_BUILD_TIMESTAMP=$(date -u -d @$SOURCE_DATE_EPOCH)")

      ls build
    '';

    buildPhase = "";

    postInstall = "";

    nativeBuildInputs = old.nativeBuildInputs ++ [
      pkg-config
      ncurses
    ];
  });
in
self
