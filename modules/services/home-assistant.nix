# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  pkgs,
  config,
  lib,
  nglib,
  ...
}:
let
  cfg = config.services.home-assistant;
  format = pkgs.formats.yaml { };

  configuration = pkgs.runCommand "home-assistant-config-dir" { } ''
    ${pkgs.remarshal}/bin/json2yaml -i ${pkgs.writeText "configuration.json" (builtins.toJSON cfg.config)} -o $out
    # Hack to support custom yaml objects,
    # i.e. secrets: https://www.home-assistant.io/docs/configuration/secrets/
    # transforms `test: '!secret rest_password'` into `test: !secret rest_password`
    sed -i -e "s/'\!\([a-z_]\+\) \(.*\)'/\!\1 \2/" $out
  '';
in
{
  options.services.home-assistant = {
    enable = lib.mkEnableOption "Enable Home Assistant";

    package = lib.mkPackageOption pkgs "home-assistant" { };

    customComponents = lib.mkOption {
      type = lib.types.attrsOf (
        lib.types.oneOf [
          lib.types.package
          lib.types.str
        ]
      );
      default = { };
      description = ''
        Extra components to be installed into <literal>/run/home-assistant/custom_components</custom_components>.
      '';
    };

    config = lib.mkOption {
      type = format.type;
      default = { };
      description = ''
        Home Assistant configuration, <link>https://www.home-assistant.io/docs/configuration/</link>.
      '';
    };

    user = lib.mkOption {
      description = "Home Assistant user.";
      type = lib.types.str;
      default = "home-assistant";
    };

    group = lib.mkOption {
      description = "Home Assistant group.";
      type = lib.types.str;
      default = "home-assistant";
    };

    envsubst = lib.mkEnableOption "Run envsubst on the configuration file.";
  };

  config = lib.mkIf cfg.enable {
    services.home-assistant.config = {
      http.server_port = lib.mkDefault "8123";
    };

    init.services.home-assistant = {
      tmpfiles = with nglib.nottmpfiles.dsl; [
        (d "/var/home-assistant/" "0700" "home-assistant" "home-assistant" _ _)
      ];

      user = cfg.user;
      group = cfg.group;

      environment.PYTHONPATH = cfg.package.pythonPath or "";

      script = pkgs.writeShellScript "home-assistant-run" ''
        ${
          if !cfg.envsubst then
            ''
              install --mode=0755 --user=${cfg.user} --group=${cfg.group} ${configuration} /var/home-assistant/configuration.yaml
            ''
          else
            ''
              ${pkgs.envsubst}/bin/envsubst \
                < ${configuration} \
                > /var/home-assistant/configuration.yaml
            ''
        }

        ${lib.optionalString (cfg.customComponents != { }) "mkdir -p /var/home-assistant/custom_components"}
        ${lib.concatStringsSep "\n" (
          lib.mapAttrsToList (
            n: v: "ln --symbolic --force --no-dereference ${v} /var/home-assistant/custom_components/${n}"
          ) cfg.customComponents
        )}

        exec ${cfg.package}/bin/hass --config /var/home-assistant
      '';
      enabled = true;
    };

    environment.systemPackages = [ cfg.package ];

    users.users.${cfg.user} = nglib.mkDefaultRec {
      description = "Home Assistant";
      group = cfg.group;
      createHome = false;
      home = "/var/empty";
      useDefaultShell = true;
      uid = config.ids.uids.home-assistant;
    };

    users.groups.${cfg.group} = nglib.mkDefaultRec { gid = config.ids.gids.home-assistant; };
  };
}
