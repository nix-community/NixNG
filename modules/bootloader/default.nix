{ config, lib, ... }:
with lib;
let
  cfg = config.bootloader;

  initrdAlgos = lib.genAttrs [
    "gzip" "bzip2" "lzma" "xz" "lzo" "lz4"
  ] (algo: "CONFIG_RD_${lib.toUpper algo}");

  filesystems = lib.genAttrs [
    "ext4" "ext3" "vfat" "xfs" "btrfs" "f2fs" "zfs"
  ] (fs:
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
      type = types.attrsOf (types.enum ["y" "n" "m"]);
      default = {};
    };

    initrdCompression = mkOption {
      description = "Supported compression algorithms for initrd";
      type = types.listOf (types.enum (builtins.attrNames initrdAlgos));
      default = [];
    };

    filesystems = mkOption {
      description = "Supported filesystems for initrd";
      type = types.listOf (types.enum (builtins.attrNames filesystems));
      default = [];
    };
  };

  config.bootloader = mkIf cfg.enable {
    kernelExtraConfig =
      (builtins.listToAttrs
        (map (algo:
          { name = initrdAlgos."${algo}"; value = "y"; }
        ) cfg.initrdCompression
      ++ map (fs:
        { name = filesystems."${fs}"; value = "y"; }
      ) cfg.filesystems));
  };
}
