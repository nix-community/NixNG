# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, nglib, ... }:
with nglib; with lib;
let
  cfg = config.services.mosquitto;

  format =
    { type = with types;
        let self = attrsOf (oneOf [ str path bool int (listOf (oneOf [ str path bool int self ])) ]);
        in self // { description = "attrs of str, path, bool, int or attrs of self"; };
      generate = config:
        pkgs.writeTextFile
          { name = "mosquitto.conf";
            text = let
              self = config:
                mapAttrsToList
                  (n: v:
                    if isString v || isInt v then
                      "${n} ${toString v}\n"
                    else if isBool v then
                      "${n} ${if v then "true" else "false"}\n"
                    else if isList v then
                      if length v != 2 then
                        throw "Invalid subtree list length of ${length v}"
                      else if (isString (elemAt v 0) || isInt (elemAt v 0)) && isAttrs (elemAt v 1) then
                        "${n} ${toString (elemAt v 0)}\n${concatStringsSep " " (self (elemAt v 1))}"
                      else if (isBool (elemAt v 0)) && isAttrs (elemAt v 1) then
                        "${n} ${if elemAt v 0 then "true" else "false"}\n${concatStringsSep " " (self (elemAt v 1))}"
                      else
                        throw "Invalid subtree content"
                    else
                      throw "unreachable"
                  )
                  config;
              in self config;
          };
    };
in
{
  options.services.mosquitto = {
    enable = mkEnableOption "Enable Mosquitto MQTT broker.";

    package = mkOption {
      type =  with types; package;
      default = pkgs.mosquitto;
      description = ''
        Which mosquitto package to use.
      '';
    };

    config = mkOption {
      type = format.type;
      default = {};
      description = ''
        Mosquitto configuration, <link>https://mosquitto.org/man/mosquitto-conf-5.html</link>.
      '';
    };

    user = mkOption {
      description = "mosquitto user.";
      type = types.str;
      default = "mosquitto";
    };

    group = mkOption {
      description = "mosquitto group.";
      type = types.str;
      default = "mosquitto";
    };

    envsubst = mkEnableOption "Run envsubst on the configuration file.";
  };

  config = mkIf cfg.enable {
    services.mosquitto.config = mkDefaultRec
      {
        persistence = true;
        persistence_location = "/var/mosquitto";
        listener =
          ["1883 0.0.0.0"
           ({ allow_anonymous = true;
           })
          ];
      };

    init.services.mosquitto = {
      script = pkgs.writeShellScript "mosquitto-run"
        ''
          mkdir -p /var/mosquitto/ /run/mosquitto
          cp ${format.generate cfg.config} /run/mosquitto/configuration.yaml
          ${optionalString cfg.envsubst
            ''
              rm /run/mosquitto/configuration.yaml
              ${pkgs.envsubst}/bin/envsubst \
                < '${format.generate cfg.config}' \
                > /run/mosquitto/configuration.yaml
            ''
          }

          chown -R ${cfg.user}:${cfg.group} /var/mosquitto/
          chmod -R u=rwX,g=r-X,o= /var/mosquitto/

          chpst -u ${cfg.user}:${cfg.group} -b mosquitto ${cfg.package}/bin/mosquitto -c /run/mosquitto/configuration.yaml
        '';
      enabled = true;
    };

    environment.systemPackages = with pkgs; [ cfg.package ];

    users.users.${cfg.user} = mkDefaultRec {
      description = "mosquitto";
      group = cfg.group;
      createHome = false;
      home = "/var/empty";
      useDefaultShell = true;
      uid = config.ids.uids.mosquitto;
    };

    users.groups.${cfg.group} = mkDefaultRec {
      gid = config.ids.gids.mosquitto;
    };
  };
}
