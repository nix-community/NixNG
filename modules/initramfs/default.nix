{ pkgs, lib, config, nixng, system, ... }:
with lib;
let
  cfg = config.initramfs;
in
{
  options.initramfs = {
    enable = mkEnableOption "Enable initramfs generation";
    config = mkOption {
      description = "Configuration for the initramfs.";
      type = types.unspecified;
      default = {
        initrd.enable = true;
      };
    };
    image = mkOption {
      description = "initramfs image.";
      type = types.unspecified;
      readOnly = true;
    };
  };

  config.initramfs.image = mkIf cfg.enable (nixng.lib.buildSystem {
    inherit system;
    config = cfg.config;
  }).initramfs;
}
