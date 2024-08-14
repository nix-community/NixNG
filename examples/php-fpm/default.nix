# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-php-fpm";
  config = ({ pkgs, config, ... }:
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = { };
        };

        init.services.apache2 = {
          shutdownOnExit = true;
          ensureSomething.link."documentRoot" = {
            src = ./documentRoot;
            dst = "/var/www";
          };
        };

        services.php-fpm = {
          pools = {
            main = {
              createUserGroup = false;
              fpmSettings = {
                "pm" = "dynamic";
                "pm.max_children" = 75;
                "pm.start_servers" = 10;
                "pm.min_spare_servers" = 5;
                "pm.max_spare_servers" = 20;
                "pm.max_requests" = 500;
              };
            };
          };
        };

        services.apache2 = {
          enable = true;
          envsubst = true;
          configuration = [
            {
              LoadModule = [
                [ "mpm_event_module" "modules/mod_mpm_event.so" ]
                [ "log_config_module" "modules/mod_log_config.so" ]
                [ "unixd_module" "modules/mod_unixd.so" ]
                [ "authz_core_module" "modules/mod_authz_core.so" ]
                [ "dir_module" "modules/mod_dir.so" ]
                [ "mime_module" "modules/mod_mime.so" ]
                [ "proxy_module" "modules/mod_proxy.so" ]
                [ "proxy_fcgi_module" "modules/mod_proxy_fcgi.so" ]
              ];
            }
            {
              Listen = "0.0.0.0:80";

              ServerRoot = "/var/www";
              ServerName = "blowhole";
              PidFile = "/httpd.pid";

              DocumentRoot = "/var/www";

              User = "www-data";
              Group = "www-data";
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
                  ProxyPassMatch =
                    [
                      "^/(.*\.php(/.*)?)$"
                      "unix:${config.services.php-fpm.pools.main.socket}|fcgi://localhost/var/www/"
                    ];

                  Directory = {
                    "/var/www" = {
                      Require = [ "all" "granted" ];
                      Options = [ "-Indexes" "+FollowSymlinks" ];
                      DirectoryIndex = "\${DIRECTORY_INDEX:-index.html}";
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
