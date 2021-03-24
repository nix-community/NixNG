{ pkgs, lib, config, options, nglib, system, ... }:
with lib;
let
  cfg = config.initramfs;
in
{
  options.initramfs = {
    enable = mkEnableOption "Enable initramfs generation";
    config = mkOption {
      description = "Configuration for the initramfs.";
      type = types.submoduleWith {
        modules = (import ../default.nix).initramfs;
        specialArgs = { inherit nglib pkgs; };
      };
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

  config.initramfs.image = mkIf cfg.enable (nglib.makeSystem {
    inherit system;
    config = cfg.config;
    name = "initrd";
  }).initramfs;
}
