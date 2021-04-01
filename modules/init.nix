{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.init;
in
{
  options.init = {
    type = mkOption {
      description = "Selected init system.";
      type = types.enum cfg.type;
    };
    availableInits = mkOption {
      description = "List of available init systems.";
      type = types.listOf types.str;
    };
    script = mkOption {
      description = "init script.";
      type = types.path;
    };
    services = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          dependencies = mkOption {
            description = "Service dependencies";
            type = types.listOf (types.str);
            default = [];
          };

          script = mkOption {
            description = "Service script";
            type = types.path;
            default = "";
          };
        };
      });
      description = "Service definitions";
      default = {};
    };
  };

  config = {
      # TODO add assertions for this module
    };
}
