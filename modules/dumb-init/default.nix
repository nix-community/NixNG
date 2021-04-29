{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.dumb-init;
  cfgRunit = config.runit;
  cfgSystem = config.system;
  cfgUsers = config.users;
  cfgNix = config.nix;

  userShell =
    let
      user = config.users.users."${cfg.type.shell.user}";
    in
      if user.useDefaultShell then
        config.users.defaultUserShell
      else
        user.shell;
in
{
  options.dumb-init = {
    enable = mkEnableOption "Enable the dumb-init init system";

    pkg = mkOption {
      description = "The dumb-init package to use";
      type = types.package;
      default = pkgs.dumb-init;
    };

    type = mkOption {
      description = "Which type of stage 2 init to run";
      type = types.submodule {
        options = {
          services = mkOption {
            description = "Run the runit stage-2 script to start runsvdir and all the services.";
            type = with types; nullOr (submodule {});
            default = null;
          };

          shell = mkOption {
            description = "Run a bash shell, without any services.";
            type = with types; nullOr (submodule {
              options = {
                user = mkOption {
                  description = "Which user to start the shell under.";
                  type = str;
                  default = "root";
                };
              };
            });
            default = null;
          };
        };
      };
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
        script =
          let
            runit = pkgs.writeShellScript "init"
              ''
                export PATH=${pkgs.busybox}/bin
                _system_config="@systemConfig@"
                
                "$_system_config/activation"                 
                exec ${cfg.pkg}/bin/dumb-init -- ${cfgRunit.stages.stage-2} 
              '';
            shell = pkgs.writeShellScript "init"
              ''
                export PATH=${pkgs.busybox}/bin:${pkgs.bash}/bin
                _system_config="@systemConfig@"

                "$_system_config/activation"                 
                . /etc/profile
                exec ${cfg.pkg}/bin/dumb-init -- su ${cfg.type.shell.user} -c ${userShell}
              '';
          in
            if cfg.type.services != null then
              runit
            else if cfg.type.shell != null then
              shell
            else
              throw "Assertion should have caught this, only one dumb-init type selected.";
      })
    ];

    assertions = [
      {
        assertion = count (x: x) (mapAttrsToList (n: v: v != null) cfg.type) == 1;
        message = "Please select exactly one dumb-init type.";
      }
    ] ++ (optional (cfg.type.shell != null)
      {
        assertion = cfgUsers.users ? "${cfg.type.shell.user}";
        message = "User ${cfg.type.shell.user} does not exist!";
      });
  };
}
