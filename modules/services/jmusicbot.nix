/*
  * NixNG
  * Copyright (c) 2021  GPL Magic_RB <magic_rb@redalder.org>
  *
  *  This file is free software: you may copy, redistribute and/or modify it
  *  under the terms of the GNU General Public License as published by the
  *  Free Software Foundation, either version 3 of the License, or (at your
  *  option) any later version.
  *
  *  This file is distributed in the hope that it will be useful, but
  *  WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  *  General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.jmusicbot;
in
{
  options.services.jmusicbot = {
    enable = mkEnableOption "Enable JMusicBot";

    package = mkOption {
      description = "JMusicBot package to use.";
      default = pkgs.jmusicbot;
      type = types.package;
    };

    config = mkOption {
      description = ''
        JMusicBot configuration file in Nix form. <link>https://github.com/jagrosh/MusicBot/wiki/Example-Config</link>
        This configuration file will be ran through envsubst, therefore "''${ENV_VAR}" works as you'd expect.
      '';
      example = literalExample ''
        {
          token = "BOT_TOKEN";
          owner = 1254789614;
          songinstatus = false;

          aliases = {
            settings = [ "status" ];
          };
        }
      '';
      type = with types; attrsOf (oneOf [ str int bool (attrsOf (listOf str)) ]);
      apply = x:
        pkgs.writeText "jmusicbot-config.txt"
          (concatMapStringsSep "\n"
            (
              { name, value }:
              if isString value then
                "${name} = \"${value}\""
              else if isInt value then
                "${name} = ${toString value}"
              else if isBool value then
                if value then
                  "${name} = true"
                else
                  "${name} = false"
              else if isAttrs value then
                "${name} {"
                +
                concatMapStringsSep "\n" (
                  { name, value }:
                  "${name} = [ ${concatStringsSep ", " value} ]"
                )
                +
                "}"
              else
                throw "Type error"
            )
            (mapAttrsToList nameValuePair x));
    };

    user = mkOption {
      description = "User to run JMusicBot under.";
      default = "jmusicbot";
      type = types.str;
    };

    group = mkOption {
      description = "Group to run JMusicBot under.";
      default = "jmusicbot";
      type = types.str;
    };
  };

  config = mkIf cfg.enable {
    users.users.${cfg.user} = mapAttrs (_: mkDefault) {
      description = "JMusicBot";
      group = cfg.group;
      home = "/var/empty";
      useDefaultShell = true;
      createHome = false;
      uid = config.ids.uids.jmusicbot;
    };

    users.groups.${cfg.group} = {
      gid = mkDefault config.ids.gids.jmusicbot;
    };

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
