{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.system;
  activation = config.activation;
in
{
  options.system = {
    environment = mkOption {
      description = "Things to be included in the root of the system.";
      type = types.submodule {
        options = {
          packages = mkOption {
            type = types.listOf types.package;
            description = "Packages to be included in the root of the system.";
            default = [];
          };
          files = mkOption {
            type = types.listOf (types.submodule {
              options = {
                source = mkOption {
                  type = types.path;
                  description = "File source.";
                };
                destination = mkOption {
                  type = types.path;
                  description = "File destination.";
                };
                copy = mkOption {
                  type = types.bool;
                  description = "Whether the file should be symlinked or copied.";
                  default = false;
                };
              };
            });
            description = "Files to be included in the root of the system.";
            default = [];
          };
        };
      };
      default = {};
    };
  };

  config = {
    assertions = [
      {
        assertion = activation.enable && cfg.environment != {};
        message = "For `system.environment` to work, the activation script must be enabled.";
      }
    ];
  };
}
