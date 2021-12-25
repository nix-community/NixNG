# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.zigbee2mqtt;
  format = pkgs.formats.yaml {};

  configDir = pkgs.runCommandNoCC "zigbee2mqtt-config-dir" {}
    ''
      mkdir -p $out
      ln -s ${format.generate "configuration.yaml" cfg.config} $out/configuration.yaml
    '';
in
{
  options.services.zigbee2mqtt = {
    enable = mkEnableOption "Enable zigbee2mqtt.";

    package = mkOption {
      type =  with types; package;
      default = pkgs.zigbee2mqtt;
      description = ''
        Which zigbee2mqtt package to use.
      '';
    };

    config = mkOption {
      type = format.type;
      default = {};
      description = ''
        Home Assistant configuration, <link>https://www.home-assistant.io/docs/configuration/</link>.
      '';
    };
  };

  config = {
    services.zigbee2mqtt.config = mapAttrsRecursive (_: v: mkDefault v)
      {
        http.server_port = "8123";
      };

    init.services.zigbee2mqtt = mkIf cfg.enable {
      script = pkgs.writeShellScript "zigbee2mqtt-run"
        ''
          mkdir -p /run/zigbee2mqtt/ /var/zigbee2mqtt/
          cp '${configDir}'/* /run/zigbee2mqtt/

          ZIGBEE2MQTT_DATA="/run/zigbee2mqtt" ${cfg.package}/bin/zigbee2mqtt
        '';
      enabled = true;
    };
  };
}
