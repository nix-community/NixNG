# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  pkgs,
  lib,
  config,
  ...
}:
let
  cfg = config.dumb-init;
  cfgRunit = config.runit;
  cfgUsers = config.users;

  userShell =
    let
      user = config.users.users."${cfg.type.shell.user}";
    in
    if user.useDefaultShell then config.users.defaultUserShell else user.shell;

  sigellConfig = overrides: pkgs.writeText "sigell.json" (builtins.toJSON (cfg.sigell // overrides));
in
{
  options.dumb-init = {
    enable = lib.mkEnableOption "Enable the dumb-init init system";

    package = lib.mkOption {
      description = "The dumb-init package to use";
      type = lib.types.package;
      default = pkgs.dumb-init;
    };

    sigell = lib.mkOption {
      description = ''
        A signal rewriting program, which allows to redirect,
        rewrite and handle signals easily
      '';
      type = with lib.types; nullOr attrs;
      default = null;
    };

    type = lib.mkOption {
      description = "Which type of stage 2 init to run";
      type = lib.types.submodule {
        options = {
          services = lib.mkOption {
            description = "Run the runit stage-2 script to start runsvdir and all the services.";
            type = with lib.types; nullOr (submodule { });
            default = null;
          };

          shell = lib.mkOption {
            description = "Run a bash shell, without any services.";
            type =
              with lib.types;
              nullOr (submodule {
                options = {
                  user = lib.mkOption {
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
  };

  config = {
    init = lib.mkMerge [
      { availableInits = [ "dumb-init" ]; }
      (lib.mkIf cfg.enable {
        type = "dumb-init";
        shutdown = pkgs.writeShellScript "dum-init-shutdown" ''
          kill -SIGHUP 1
        '';
        script =
          let
            runit = pkgs.writeShellScript "init" ''
              export PATH=${pkgs.busybox}/bin
              _system_config="@systemConfig@"

              function _sig_hup() {
                kill -HUP $_init_system

                echo "Shutting down"

                _all_pids="$(ps aux | grep "[r]unsv " | tr -s ' ' | cut -f 2 -d ' ')"
                start_ts=$(date +%s)
                while ! [ -z "$_all_pids" ] ; do
                    now_ts=$(date +%s)
                    if [ $(( now_ts - start_ts )) -gt 60 ]; then
                        echo "Shutdown timeout."
                        break
                    fi
                    _all_pids="$(ps aux | grep "[r]unsv " | tr -s ' ' | cut -f 2 -d ' ')"
                    echo $_all_pids
                    sleep 1
                done

                exit 0
              }
              trap _sig_hup SIGHUP

              "$_system_config/activation"
              # exec ${cfg.package}/bin/dumb-init --rewrite 15:1 --single-child -- \
              ${sigell [ "${cfgRunit.stages.stage-2}" ]} &
              _init_system="$!"
              wait "$_init_system"

            '';
            shell = pkgs.writeShellScript "init" ''
              export PATH=${pkgs.busybox}/bin:${pkgs.bash}/bin
              _system_config="@systemConfig@"

              "$_system_config/activation"
              . /etc/profile
              exec ${cfg.package}/bin/dumb-init -- \
                ${sigell [
                  "su"
                  "${cfg.type.shell.user}"
                  "-c"
                  "${userShell} \${1+-c} \"\${*@Q}\""
                ]}
            '';
            sigell =
              cmd:
              if cfg.sigell != null then
                "${pkgs.sigell}/bin/sigell ${sigellConfig { command = cmd; }}"
              else
                lib.concatStringsSep " " cmd;
          in
          if cfg.type.services != null then
            runit
          else if cfg.type.shell != null then
            shell
          else
            throw "Assertion should have caught this, only one dumb-init type selected.";
      })
    ];

    assertions = lib.mkIf cfg.enable (
      [
        {
          assertion = lib.count (x: x) (lib.mapAttrsToList (n: v: v != null) cfg.type) == 1;
          message = "Please select exactly one dumb-init type.";
        }
      ]
      ++ (lib.optional (cfg.type.shell != null) {
        assertion = cfgUsers.users ? "${cfg.type.shell.user}";
        message = "User ${cfg.type.shell.user} does not exist!";
      })
    );
  };
}
