nglib:
(nglib "x86_64-linux").makeSystem {
  system = "x86_64-linux";
  name = "nixng-nginx";
  config = ({ pkgs, config, ... }:
    {
      config = {
        dumb-init = {
          enable = true;
          type.services = {};
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
