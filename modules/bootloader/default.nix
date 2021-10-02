/*
  * NixNG
  * Copyright (c) 2021  GPL Magic_RB <magic_rb@redalder.org>   
  *  
  *  This file is free software: you may copy, redistribute and/or modify it  
  *  under the terms of the GNU General Public License as published by the  
  *  Free Software Foundation, either version 3 of the License, or (at your  
  *  option) any later version.  
  *  
  *  This file is distributed in the hope that it will be useful, but  
  *  WITHOUT ANY WARRANTY; without even the implied warranty of  
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU  
  *  General Public License for more details.  
  *  
  *  You should have received a copy of the GNU General Public License  
  *  along with this program.  If not, see <http://www.gnu.org/licenses/>.  
*/

{ config, lib, ... }:
with lib;
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
    enable = mkEnableOption "Enable the bootloader";
    kernelExtraConfig = mkOption {
      description = "";
      type = types.attrsOf (types.enum [ "y" "n" "m" ]);
      default = { };
    };

    initrdCompression = mkOption {
      description = "Supported compression algorithms for initrd";
      type = types.listOf (types.enum (builtins.attrNames initrdAlgos));
      default = [ ];
    };

    filesystems = mkOption {
      description = "Supported filesystems for initrd";
      type = types.listOf (types.enum (builtins.attrNames filesystems));
      default = [ ];
    };
  };

  config.bootloader = mkIf cfg.enable {
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
