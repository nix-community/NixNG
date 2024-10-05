# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, ... }:
let
  cfg = config.services.pantalaimon;

  dataDir = "/var/lib/pantalaimon";
in
{
  options.services.pantalaimon = {
    enable = lib.mkEnableOption "Enable pantalaimon";

    package = lib.mkOption {
      description = "Pantalaimon package to use";
      default = pkgs.pantalaimon.override { enableDbusUi = false; };
      type = lib.types.package;
    };

    config = lib.mkOption {
      description = ''
        Pantalaimon configuration file in Nix form. <link>https://github.com/matrix-org/pantalaimon/blob/master/docs/man/pantalaimon.5.md</link>.
      '';
      example = lib.literalExample ''
        {
          Default =
            {
              LogLevel = "Debug";
              SSL = "True";
              Notifications = "On";
            };

          Clocktown =
            {
              Homeserver = "https://localhost:8448";
              ListenAddress = "localhost"
              ListenPort = 8009
              Proxy = "http://localhost:8080";
              SSL = "False";
            };
        }
      '';
      type = with lib.types; attrsOf (attrsOf (oneOf [ string int ]));
      apply = x: with pkgs; writeText "pantalaimon.conf" (generators.toINI { } x);
    };

    user = lib.mkOption {
      description = "User to run Pantalaimon under.";
      default = "pantalaimon";
      type = lib.types.str;
    };

    group = lib.mkOption {
      description = "Group to run Pantalaimon under.";
      default = "pantalaimon";
      type = lib.types.str;
    };
  };

  config = lib.mkIf cfg.enable {
    users.users.${cfg.user} = lib.mapAttrs (_: lib.mkDefault) {
      description = "Pantalaimon";
      group = cfg.group;
      home = "/var/empty";
      createHome = false;
      uid = config.ids.uids.pantalaimon;
    };

    users.groups.${cfg.group} = {
      gid = lib.mkDefault config.ids.gids.pantalaimon;
    };

    environment.systemPackages = [ cfg.package ];

    init.services.pantalaimon =
      {
        ensureSomething.create."dataDir" = {
          type = "directory";
          mode = "770";
          owner = "${cfg.user}:${cfg.group}";
          dst = dataDir;
          persistent = true;
        };

        script = pkgs.writeShellScript "pantalaimon-run"
          ''
            echo AAAA
            ${cfg.package}/bin/pantalaimon -c ${cfg.config}
          '';
        enabled = true;
      };
  };
}
