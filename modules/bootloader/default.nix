# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ config, lib, ... }:
let
  cfg = config.bootloader;

  initrdAlgos = lib.genAttrs [
    "gzip"
    "bzip2"
    "lzma"
    "xz"
    "lzo"
    "lz4"
  ]
    (algo: "CONFIG_RD_${lib.toUpper algo}");

  filesystems = lib.genAttrs [
    "ext4"
    "ext3"
    "vfat"
    "xfs"
    "btrfs"
    "f2fs"
    "zfs"
  ]
    (fs:
      if fs == "zfs" then
        throw "Not supported :) would need a patch"
      else
        "CONFIG_${lib.toUpper fs}_FS"
    );
in
{
  options.bootloader = {
    enable = lib.mkEnableOption "Enable the bootloader";
    kernelExtraConfig = lib.mkOption {
      description = "";
      type = with lib.types; attrsOf (enum [ "y" "n" "m" ]);
      default = { };
    };

    initrdCompression = lib.mkOption {
      description = "Supported compression algorithms for initrd";
      type = with lib.types; listOf (enum (builtins.attrNames initrdAlgos));
      default = [ ];
    };

    filesystems = lib.mkOption {
      description = "Supported filesystems for initrd";
      type = with lib.types; listOf (enum (builtins.attrNames filesystems));
      default = [ ];
    };
  };

  config.bootloader = lib.mkIf cfg.enable {
    kernelExtraConfig =
      (builtins.listToAttrs
        (map
          (algo:
            { name = initrdAlgos."${algo}"; value = "y"; }
          )
          cfg.initrdCompression
        ++ map
          (fs:
            { name = filesystems."${fs}"; value = "y"; }
          )
          cfg.filesystems));
  };
}
