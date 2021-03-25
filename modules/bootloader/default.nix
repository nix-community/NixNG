{ config, lib, ... }:
with lib;
let
  cfg = config.bootloader;
in
{
  options.bootloader = {
    enable = mkEnableOption "Enable the bootloader";
    config = mkOption {
      description = "";
      type = types.submodule {
        options = {
          kernelConfiguration = mkOption {
            description = "";
            type = types.attrsOf (types.enum ["y" "n" "m"]);
            default = {};
          };
        };
      };
      default = {};
    };
  };

  config.bootloader = mkIf cfg.enable {};
}
