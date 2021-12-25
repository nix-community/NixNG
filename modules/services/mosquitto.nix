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
                  (traceValSeq config);
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
  };

  config = {
    services.mosquitto.config = mapAttrsRecursive (_: v: mkDefault v)
      {
        persistence = true;
        persistence_location = "/var/mosquitto";
        listener =
          ["1883 0.0.0.0"
           ({ allow_anonymous = true;
           })
          ];
      };

    init.services.mosquitto = mkIf cfg.enable {
      script = pkgs.writeShellScript "mosquitto-run"
        ''
          mkdir -p /var/mosquitto/
          ${cfg.package}/bin/mosquitto -c ${format.generate cfg.config}
        '';
      enabled = true;
    };
  };
}
