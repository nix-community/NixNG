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
  cfg = config.services.nginx;
  runtimeConfig = "/run/cfg/nginx.cfg";

  inherit (nglib.generators) toNginx;
in
{
  options = {
    services.nginx = {
      enable = mkEnableOption "Enable Nginx http server.";
      package = mkOption {
        description = "Nginx package.";
        type = types.package;
        default = pkgs.nginx;
      };
      user = mkOption {
        description = "Nginx user.";
        type = types.str;
        default = "nginx";
      };
      group = mkOption {
        description = "Nginx group.";
        type = types.str;
        default = "nginx";
      };
      envsubst = mkEnableOption "Run envsubst on the configuration file.";
      configuration = mkOption {
        description = "Nginx configuration";
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
                (listOf (oneOf [ str self]))
              ];
          in
            self // { description = "loop breaker"; };
      };
    };
  };

  config = mkIf cfg.enable
    {
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
            script = pkgs.writeShellScript "nginx-run"
              (if cfg.envsubst then
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
              '');
            enabled = true;
          };

      users.users.${cfg.user} = mkDefault {
        description = "Nginx";
        group = cfg.group;
        createHome = false;
        home = "/var/empty";
        useDefaultShell = true;
        uid = config.ids.uids.nginx;
      };

      users.groups.${cfg.group} = mkDefault {
        gid = config.ids.gids.nginx;
      };
    };
}
