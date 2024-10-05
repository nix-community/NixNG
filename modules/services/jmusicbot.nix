# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, ... }:
let
  cfg = config.services.jmusicbot;
in
{
  options.services.jmusicbot = {
    enable = lib.mkEnableOption "Enable JMusicBot";

    package = lib.mkOption {
      description = "JMusicBot package to use.";
      default = pkgs.jmusicbot;
      type = lib.types.package;
    };

    config = lib.mkOption {
      description = ''
        JMusicBot configuration file in Nix form. <link>https://github.com/jagrosh/MusicBot/wiki/Example-Config</link>
        This configuration file will be ran through envsubst, therefore "''${ENV_VAR}" works as you'd expect.
      '';
      example = lib.literalExample ''
        {
          token = "BOT_TOKEN";
          owner = 1254789614;
          songinstatus = false;

          aliases = {
            settings = [ "status" ];
          };
        }
      '';
      type = with lib.types; attrsOf (oneOf [ str int bool (attrsOf (listOf str)) ]);
      apply = x:
        pkgs.writeText "jmusicbot-config.txt"
          (lib.concatMapStringsSep "\n"
            (
              { name, value }:
              if lib.isString value then
                "${name} = \"${value}\""
              else if lib.isInt value then
                "${name} = ${toString value}"
              else if lib.isBool value then
                if value then
                  "${name} = true"
                else
                  "${name} = false"
              else if lib.isAttrs value then
                "${name} {"
                +
                lib.concatMapStringsSep "\n" (
                  { name, value }:
                  "${name} = [ ${lib.concatStringsSep ", " value} ]"
                )
                +
                "}"
              else
                throw "Type error"
            )
            (lib.mapAttrsToList lib.nameValuePair x));
    };

    user = lib.mkOption {
      description = "User to run JMusicBot under.";
      default = "jmusicbot";
      type = lib.types.str;
    };

    group = lib.mkOption {
      description = "Group to run JMusicBot under.";
      default = "jmusicbot";
      type = lib.types.str;
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.user} = lib.mapAttrs (_: lib.mkDefault) {
      description = "JMusicBot";
      group = cfg.group;
      home = "/var/empty";
      useDefaultShell = true;
      createHome = false;
      uid = config.ids.uids.jmusicbot;
    };

    users.groups.${cfg.group} = {
      gid = lib.mkDefault config.ids.gids.jmusicbot;
    };

    environment.systemPackages = [ cfg.package ];

    init.services.jmusicbot =
      {
        script = pkgs.writeShellScript "jmusicbot-run"
          ''
            mkdir -p /run/cfg/jmusicbot
            ${pkgs.envsubst}/bin/envsubst < ${cfg.config} > /run/cfg/jmusicbot/config.txt

            cd /run/cfg/jmusicbot
            # chpst -u ${cfg.user}:${cfg.group}
            ${cfg.package}/bin/JMusicBot
          '';
        enabled = true;
      };
  };
}
