{
  lib,
  config,
  pkgs,
  ...
}:
{
  options = {
    nixos.services.nginx = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
      };

      virtualHosts = lib.mkOption {
        type = lib.types.attrsOf (
          lib.types.submodule {
            options = {
              locations = lib.mkOption {
                type = lib.types.attrsOf (
                  lib.types.submodule {
                    options.proxyPass = lib.mkOption { type = lib.types.str; };

                    options.proxyWebsockets = lib.mkOption {
                      type = lib.types.bool;
                      default = false;
                    };

                    options.extraConfig = lib.mkOption {
                      type = lib.types.lines;
                      default = "";
                    };
                  }
                );
                default = { };
              };

              forceSSL = lib.mkOption {
                type = lib.types.bool;
                default = false;
              };

              addSSL = lib.mkOption {
                type = lib.types.bool;
                default = false;
              };

              useHTTPS = lib.mkOption {
                type = lib.types.bool;
                default = false;
              };
            };
          }
        );
        default = { };
      };
    };
  };

  config = {
    services.nginx = {
      enable = config.nixos.services.nginx.enable;
      envsubst = config.nixos.services.nginx.enable;

      configuration = lib.singleton {
        daemon = "off";
        worker_processes = 2;
        user = "nginx";

        events."" = {
          use = "epoll";
          worker_connections = 128;
        };

        error_log = [
          "/dev/stderr"
          "warn"
        ];

        pid = "/nginx.pid";

        http."" =
          [
            {
              server_tokens = "off";
              include = [ [ "${pkgs.nginx}/conf/mime.types" ] ];
              charset = "utf-8";
              access_log = [
                "/dev/stdout"
                "combined"
              ];
            }
          ]
          ++ (lib.flip lib.mapAttrsToList config.nixos.services.nginx.virtualHosts (
            server_name: server: {
              server."" = {
                listen = [
                  "80"
                  "http2"
                ];
                inherit server_name;

                location = lib.flip lib.mapAttrs server.locations (
                  location: settings: [
                    { proxy_pass = settings.proxyPass; }
                    (lib.optionalAttrs settings.proxyWebsockets {
                      proxy_set_header = [
                        [
                          "Host"
                          "$$host"
                        ]
                        [ "X-Real-IP $$remote_addr" ]
                        [ "X-Forwarded-For $$proxy_add_x_forwarded_for" ]
                        [ "X-Forwarded-Proto $$scheme" ]
                        [ "Upgrade $$http_upgrade" ]
                        [ "Connection upgrade" ]
                      ];
                    })
                    settings.extraConfig
                  ]
                );
              };
            }
          ));
      };
    };
  };
}
