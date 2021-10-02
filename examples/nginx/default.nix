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
  name = "nixng-nginx";
  config = ({ pkgs, config, ... }:
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = { };
        };
        init.services.nginx = {
          shutdownOnExit = true;
          ensureSomething.link."documentRoot" = {
            src = "${pkgs.apacheHttpd}/htdocs";
            dst = "/var/www";
          };
        };
        services.nginx = {
          enable = true;
          envsubst = true;
          configuration = [
            {
              daemon = "off";
              worker_processes = 2;
              user = "nginx";

              events."" = {
                use = "epoll";
                worker_connections = 128;
              };

              error_log = [ "/dev/stderr" "info" ];
              pid = "/nginx.pid";

              http."" = {
                server_tokens = "off";
                include = "${pkgs.nginx}/conf/mime.types";
                charset = "utf-8";

                access_log = [ "/dev/stdout" "combined" ];

                server."" = {
                  server_name = "localhost";
                  listen = "0.0.0.0:80";

                  location."/var/www" = {
                    root = "html";
                  };
                };
              };
            }
          ];
        };
      };
    });
}
