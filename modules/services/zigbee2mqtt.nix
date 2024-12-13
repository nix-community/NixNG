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
  cfg = config.services.zigbee2mqtt;
  format = pkgs.formats.yaml { };

  configDir = pkgs.runCommandNoCC "zigbee2mqtt-config-dir" { } ''
    mkdir -p $out
    ln -s ${format.generate "configuration.yaml" cfg.config} $out/configuration.yaml
  '';
in
{
  options.services.zigbee2mqtt = {
    enable = lib.mkEnableOption "Enable zigbee2mqtt.";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.zigbee2mqtt;
      description = ''
        Which zigbee2mqtt package to use.
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
      description = "zigbee2mqtt user.";
      type = lib.types.str;
      default = "zigbee2mqtt";
    };

    group = lib.mkOption {
      description = "zigbee2mqtt group.";
      type = lib.types.str;
      default = "zigbee2mqtt";
    };

    envsubst = lib.mkEnableOption "Run envsubst on the configuration file.";
  };

  config = lib.mkIf cfg.enable {
    services.zigbee2mqtt.config = nglib.mkDefaultRec { http.server_port = "8123"; };

    init.services.zigbee2mqtt = {
      script = pkgs.writeShellScript "zigbee2mqtt-run" ''
        mkdir -p /var/zigbee2mqtt/
        cp '${configDir}'/* /var/zigbee2mqtt/
        ${lib.optionalString cfg.envsubst ''
          rm /var/zigbee2mqtt/configuration.yaml
          ${pkgs.envsubst}/bin/envsubst \
            < '${configDir}/configuration.yaml' \
            > /var/zigbee2mqtt/configuration.yaml
        ''}

        chown -R ${cfg.user}:${cfg.group} /var/zigbee2mqtt/
        chmod -R u=rwX,g=r-X,o= /var/zigbee2mqtt/

        ZIGBEE2MQTT_DATA="/var/zigbee2mqtt/" chpst -u ${cfg.user}:${cfg.group} ${cfg.package}/bin/zigbee2mqtt
      '';
      enabled = true;
    };

    environment.systemPackages = [ cfg.package ];

    users.users.${cfg.user} = nglib.mkDefaultRec {
      description = "zigbee2mqtt";
      group = cfg.group;
      createHome = false;
      home = "/var/empty";
      useDefaultShell = true;
      uid = config.ids.uids.zigbee2mqtt;
    };

    users.groups.${cfg.group} = nglib.mkDefaultRec { gid = config.ids.gids.zigbee2mqtt; };
  };
}
