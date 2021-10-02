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
