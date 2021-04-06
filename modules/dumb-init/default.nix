{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.dumb-init;
  cfgRunit = config.runit;
  cfgSystem = config.system;
in
{
  options.dumb-init = {
    enable = mkEnableOption "Enable the dumb-init init system";

    pkg = mkOption {
      description = "The dumb-init package to use";
      type = types.package;
      default = pkgs.dumb-init;
    };

    runtimeServiceDirectory = mkOption {
      description = "where runsvdir should create superwise and log directories for services";
      type = types.path;
      default = "/run/sv";
    };
  };

  config = {
    init = mkMerge [
      {
        availableInits = [ "dumb-init" ];
      }
      (mkIf cfg.enable {
        type = "dumb-init";
        shutdown = pkgs.writeShellScript "dum-init-shutdown"
          ''
            kill -SIGTERM 1
          '';
        script = pkgs.writeShellScript "init"
          ''
            export PATH=${pkgs.busybox}/bin

            mkdir -p /tmp /var/empty
            ${cfgSystem.activationScript}
            exec ${cfg.pkg}/bin/dumb-init -- ${cfgRunit.stages.stage-2} 
          '';
      })
    ];
  };
}
