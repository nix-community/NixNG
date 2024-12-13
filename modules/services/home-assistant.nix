# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

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

  configDir = pkgs.runCommandNoCC "home-assistant-config-dir" { } ''
    mkdir -p $out
    ${pkgs.remarshal}/bin/json2yaml -i ${pkgs.writeText "configuration.json" (builtins.toJSON cfg.config)} -o $out/configuration.yaml
    # Hack to support custom yaml objects,
    # i.e. secrets: https://www.home-assistant.io/docs/configuration/secrets/
    sed -i -e "s/'\!\([a-z_]\+\) \(.*\)'/\!\1 \2/;s/^\!\!/\!/;" $out/configuration.yaml
  '';
in
{
  options.services.home-assistant = {
    enable = lib.mkEnableOption "Enable Home Assistant";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.home-assistant;
      description = ''
        Which home-assistant package to use. Adding components is a shorthand for adding python packages.
      '';
    };

    customComponents = lib.mkOption {
      type =
        with lib.types;
        attrsOf (oneOf [
          package
          str
        ]);
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
    services.home-assistant.config = nglib.mkDefaultRec { http.server_port = "8123"; };

    init.services.home-assistant = {
      script = pkgs.writeShellScript "home-assistant-run" ''
        mkdir -p /var/home-assistant/
        cp '${configDir}'/* /var/home-assistant/
        ${lib.optionalString cfg.envsubst ''
          rm /var/home-assistant/configuration.yaml
          ${pkgs.envsubst}/bin/envsubst \
            < '${configDir}/configuration.yaml' \
            > /var/home-assistant/configuration.yaml
        ''}

        ${if cfg.customComponents != { } then "mkdir -p /var/home-assistant/custom_components" else ""}
        ${lib.concatStringsSep "\n" (
          lib.mapAttrsToList (
            n: v: "ln -sf ${v} /var/home-assistant/custom_components/${n}"
          ) cfg.customComponents
        )}

        chown -R ${cfg.user}:${cfg.group} /var/home-assistant/
        chmod -R u=rwX,g=r-X,o= /var/home-assistant/

        export PYTHONPATH=${cfg.package.pythonPath or ""}
        chpst -u ${cfg.user}:${cfg.group} ${cfg.package}/bin/hass --config /var/home-assistant
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
