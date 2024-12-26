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
  cfg = config.services.genericDevicePlugin;
  settingsFormat = pkgs.formats.json { };
  usageLink = "https://github.com/squat/generic-device-plugin/tree/main?tab=readme-ov-file#usage";
in
{
  options.services.genericDevicePlugin = {
    enable = lib.mkEnableOption "Whether to enable the generic-device-plugin";

    package = lib.mkPackageOption pkgs "generic-device-plugin" { };

    settings = {
      devices = lib.mkOption {
        type = lib.types.listOf (settingsFormat.type);
        default = [ ];
        description = ''
          See [upstream documentation](${usageLink});
        '';
        example = lib.literalExpression ''
          [
            {
              name = "zigbee";
              groups = lib.singleton { paths = lib.singleton { path = "/dev/ttyZigbee"; }; };
            }
            {
              name = "ender3";
              groups = lib.singleton { paths = lib.singleton { path = "/dev/ttyEnder3"; }; };
            }
          ]
        '';
      };
      domain = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          See [upstream documentation](${usageLink}). If `null`, upstream default is used.
        '';
      };
      listen = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          See [upstream documentation](${usageLink}). If `null`, upstream default is used.
        '';
      };
      logLevel = lib.mkOption {
        type = lib.types.enum [
          "all"
          "debug"
          "info"
          "warn"
          "error"
          "none"
          "info"
        ];
        default = "info";
        description = ''
          Log level to use.
        '';
      };
      pluginDirectory = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = ''
          See [upstream documentation](${usageLink}). If `null`, upstream default is used.
        '';
      };
    };
    user = nglib.mkUserOption "generic-device-plugin" "User to run `generic-device-plugin` as.";
    group = nglib.mkGroupOption "generic-device-plugin" "Group to run `generic-device-plugin` as.";
  };
  config = lib.mkIf cfg.enable {

    users.users.${cfg.user} = nglib.mkDefaultRec {
      description = "generic-device-plugin";
      group = cfg.group;
      createHome = false;
      home = "/var/empty";
      useDefaultShell = false;
      uid = config.ids.uids.generic-device-plugin;
    };

    users.groups.${cfg.group} = nglib.mkDefaultRec { gid = config.ids.gids.generic-device-plugin; };

    init.services.generic-device-plugin = {
      shutdownOnExit = true;
      enabled = true;
      execStart = "${lib.getExe cfg.package} --config ${settingsFormat.generate "settings.json" cfg.settings}";
    };
  };
}
