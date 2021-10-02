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

nglib:
(nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-apache-runit";
  config = ({ pkgs, config, ... }:
    let
      ids = config.ids;
    in
    {
      config = {
        runit = {
          enable = true;
        };
        init.services.apache2 = {
          shutdownOnExit = true;
          ensureSomething.link."documentRoot" = {
            src = "${pkgs.apacheHttpd}/htdocs";
            dst = "/var/www";
          };
        };
        users.users."www-data" = {
          uid = ids.uids.www-data;
          group = "www-data";
        };
        users.groups."www-data" = {
          gid = ids.gids.www-data;
        };
        services.apache2 = {
          enable = true;
          configuration = [
            {
              LoadModule = [
                [ "mpm_event_module" "modules/mod_mpm_event.so" ]
                [ "log_config_module" "modules/mod_log_config.so" ]
                [ "unixd_module" "modules/mod_unixd.so" ]
                [ "authz_core_module" "modules/mod_authz_core.so" ]
                [ "dir_module" "modules/mod_dir.so" ]
                [ "mime_module" "modules/mod_mime.so" ]
              ];
            }
            {
              Listen = "0.0.0.0:80";

              ServerRoot = "/var/www";
              ServerName = "blowhole";
              PidFile = "/httpd.pid";

              User = "www-data";
              Group = "www-data";

              DocumentRoot = "/var/www";
            }

            {
              ErrorLog = "/dev/stderr";
              TransferLog = "/dev/stdout";

              LogLevel = "info";
            }

            {
              AddType = [
                [ "image/svg+xml" "svg" "svgz" ]
              ];
              AddEncoding = [ "gzip" "svgz" ];

              TypesConfig = "${pkgs.apacheHttpd}/conf/mime.types";
            }

            {
              Directory = {
                "/" = {
                  Require = [ "all" "denied" ];
                  Options = "SymlinksIfOwnerMatch";
                };
              };

              VirtualHost = {
                "*:80" = {
                  Directory = {
                    "/var/www" = {
                      Require = [ "all" "granted" ];
                      Options = [ "-Indexes" "+FollowSymlinks" ];
                      DirectoryIndex = "index.html";
                    };
                  };
                };
              };
            }
          ];
        };
      };
    });
}
