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
  cfg = config.services.mosquitto;

  format = {
    type =
      with lib.types;
      let
        self = attrsOf (oneOf [
          str
          path
          bool
          int
          (listOf (oneOf [
            str
            path
            bool
            int
            self
          ]))
        ]);
      in
      self // { description = "attrs of str, path, bool, int or attrs of self"; };
    generate =
      config:
      pkgs.writeTextFile {
        name = "mosquitto.conf";
        text =
          let
            self =
              config:
              lib.mapAttrsToList (
                n: v:
                if lib.isString v || lib.isInt v then
                  "${n} ${toString v}\n"
                else if lib.isBool v then
                  "${n} ${if v then "true" else "false"}\n"
                else if lib.isList v then
                  if lib.length v != 2 then
                    throw "Invalid subtree list length of ${lib.length v}"
                  else if
                    (lib.isString (lib.elemAt v 0) || lib.isInt (lib.elemAt v 0)) && lib.isAttrs (lib.elemAt v 1)
                  then
                    "${n} ${toString (lib.elemAt v 0)}\n${lib.concatStringsSep " " (self (lib.elemAt v 1))}"
                  else if (lib.isBool (lib.elemAt v 0)) && lib.isAttrs (lib.elemAt v 1) then
                    "${n} ${if lib.elemAt v 0 then "true" else "false"}\n${
                      lib.concatStringsSep " " (self (lib.elemAt v 1))
                    }"
                  else
                    throw "Invalid subtree content"
                else
                  throw "unreachable"
              ) config;
          in
          self config;
      };
  };
in
{
  options.services.mosquitto = {
    enable = lib.mkEnableOption "Enable Mosquitto MQTT broker.";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.mosquitto;
      description = ''
        Which mosquitto package to use.
      '';
    };

    config = lib.mkOption {
      type = format.type;
      default = { };
      description = ''
        Mosquitto configuration, <link>https://mosquitto.org/man/mosquitto-conf-5.html</link>.
      '';
    };

    user = lib.mkOption {
      description = "mosquitto user.";
      type = lib.types.str;
      default = "mosquitto";
    };

    group = lib.mkOption {
      description = "mosquitto group.";
      type = lib.types.str;
      default = "mosquitto";
    };

    envsubst = lib.mkEnableOption "Run envsubst on the configuration file.";
  };

  config = lib.mkIf cfg.enable {
    services.mosquitto.config = nglib.mkDefaultRec {
      persistence = true;
      persistence_location = "/var/mosquitto";
      listener = [
        "1883 0.0.0.0"
        ({ allow_anonymous = true; })
      ];
    };

    init.services.mosquitto = {
      script = pkgs.writeShellScript "mosquitto-run" ''
        mkdir -p /var/mosquitto/ /run/mosquitto
        cp ${format.generate cfg.config} /run/mosquitto/configuration.yaml
        ${lib.optionalString cfg.envsubst ''
          rm /run/mosquitto/configuration.yaml
          ${pkgs.envsubst}/bin/envsubst \
            < '${format.generate cfg.config}' \
            > /run/mosquitto/configuration.yaml
        ''}

        chown -R ${cfg.user}:${cfg.group} /var/mosquitto/
        chmod -R u=rwX,g=r-X,o= /var/mosquitto/

        chpst -u ${cfg.user}:${cfg.group} ${cfg.package}/bin/mosquitto -c /run/mosquitto/configuration.yaml
      '';
      enabled = true;
    };

    environment.systemPackages = [ cfg.package ];

    users.users.${cfg.user} = nglib.mkDefaultRec {
      description = "mosquitto";
      group = cfg.group;
      createHome = false;
      home = "/var/empty";
      useDefaultShell = true;
      uid = config.ids.uids.mosquitto;
    };

    users.groups.${cfg.group} = nglib.mkDefaultRec { gid = config.ids.gids.mosquitto; };
  };
}
