# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, config, lib, nglib, ... }:
with lib;
let
  cfg = config.services.apache2;
  runtimeConfig = "/run/cfg/apache.cfg";

  inherit (nglib.generators) toApache;
in
{
  options = {
    services.apache2 = {
      enable = mkEnableOption "Enable Apache2 http server";
      package = mkOption {
        description = "Apache2 package";
        type = types.package;
        default = pkgs.apacheHttpd;
      };
      createUserGroup = mkOption {
        description = ''
          Whether to create the default user <literal>www-data</literal>
          and group <literal>www-data</literal>.
        '';
        type = types.bool;
        default = true;
      };
      envsubst = mkEnableOption "Run envsubst on the configuration file.";
      configuration = mkOption {
        description = "Apache2 configuration";
        type = with types;
          let
            self =
              oneOf [
                (attrsOf (oneOf [
                  str
                  int
                  (listOf (oneOf [ str int (listOf (oneOf [ str int ])) ]))
                  (attrsOf self)
                ]))
                (listOf (oneOf [ str self ]))
              ];
          in
          self // { description = "loop breaker"; };
      };
    };
  };


  config = mkIf cfg.enable
    {
      init.services.apache2 =
        let
          config = pkgs.writeText "apache2.cfg" (toApache cfg.configuration);
        in
        {
          script = pkgs.writeShellScript "apache2-run"
            (if cfg.envsubst then
              ''
                export PATH=${pkgs.envsubst}/bin:$PATH

                mkdir -p /run/cfg
                install -o www-data -g www-data -m 0440 /dev/null ${runtimeConfig}
                envsubst < ${config} > ${runtimeConfig}

                HOME=~www-data ${cfg.package}/bin/httpd \
                  -f ${runtimeConfig} -DFOREGROUND 2>&1
              ''
            else
              ''
                HOME=~www-data ${cfg.package}/bin/httpd \
                  -f ${config} -DFOREGROUND 2>&1
              '');
          enabled = true;
        };

      environment.systemPackages = with pkgs; [ cfg.package ];

      users.users."www-data" = mkIf cfg.createUserGroup {
        description = "Apache HTTPD";
        group = "www-data";
        createHome = false;
        home = "/var/empty";
        useDefaultShell = true;
        uid = config.ids.uids.www-data;
      };

      users.groups."www-data" = mkIf cfg.createUserGroup {
        gid = config.ids.gids.www-data;
      };
    };
}
