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
  cfg = config.services.nginx;
  runtimeConfig = "/run/cfg/nginx.cfg";

  inherit (nglib.generators) toNginx;
in
{
  options = {
    services.nginx = {
      enable = lib.mkEnableOption "Enable Nginx http server.";
      package = lib.mkOption {
        description = "Nginx package.";
        type = lib.types.package;
        default = pkgs.nginx;
      };
      user = lib.mkOption {
        description = "Nginx user.";
        type = lib.types.str;
        default = "nginx";
      };
      group = lib.mkOption {
        description = "Nginx group.";
        type = lib.types.str;
        default = "nginx";
      };
      envsubst = lib.mkEnableOption "Run envsubst on the configuration file.";
      configuration = lib.mkOption {
        description = "Nginx configuration";
        type =
          with lib.types;
          let
            self =
              (listOf (oneOf [
                str
                (attrsOf (oneOf [
                  str
                  int
                  (listOf (oneOf [
                    str
                    int
                    (listOf (oneOf [
                      str
                      int
                    ]))
                  ]))
                  (attrsOf self)
                ]))
              ]));
          in
          self // { description = "loop breaker"; };
      };
    };
  };

  config = lib.mkIf cfg.enable {
    init.services.nginx =
      let
        config = pkgs.writeText "nginx.cfg" (toNginx cfg.configuration);
      in
      {
        ensureSomething.create."cache" = {
          type = "directory";
          mode = "750";
          owner = "${cfg.user}:${cfg.group}";
          dst = "/var/cache/nginx/";
          persistent = false;
        };
        script = pkgs.writeShellScript "nginx-run" (
          if cfg.envsubst then
            ''
              export PATH=${pkgs.envsubst}/bin:$PATH

              mkdir -p /run/cfg
              install -o nginx -g nginx -m 0440 /dev/null ${runtimeConfig}
              envsubst < ${config} > ${runtimeConfig}

              HOME=~nginx ${cfg.package}/bin/nginx \
                -c ${runtimeConfig}
            ''
          else
            ''
              HOME=~nginx ${cfg.package}/bin/nginx \
                -c ${config}
            ''
        );
        enabled = true;
      };

    environment.systemPackages = [ cfg.package ];

    users.users.${cfg.user} = nglib.mkDefaultRec {
      description = "Nginx";
      group = cfg.group;
      createHome = false;
      home = "/var/empty";
      useDefaultShell = true;
      uid = config.ids.uids.nginx;
    };

    users.groups.${cfg.group} = nglib.mkDefaultRec { gid = config.ids.gids.nginx; };
  };
}
