{
  lib,
  config,
  pkgs,
  options,
  ...
}:
let
  cfg = config.nixos.services.nginx;

  nixosOptions = options.nixos.type.getSubOptions ["nixos"];

  evalSubmoduleOption = path: options:
    let
      option = lib.getAttrFromPath path options;
    in
      lib.evalModules {
        modules = option.type.getSubModules ++ option.definitions;
        inherit
          (option.type.functor.payload)
          class
          specialArgs
          ;
      };

  extractWithPriority = options: submodulePath: optionPath:
    let
      submoduleOptions = evalSubmoduleOption submodulePath options;
      option = lib.getAttrFromPath optionPath submoduleOptions.options;
    in
      lib.mkOverride (option.highestPrio) (option.value);

  recommendedProxyConfig = {
    proxy_set_header = [
      ["Host" "$$host"]
      ["X-Real-IP" "$$remote_addr"]
      ["X-Forwarded-For" "$$proxy_add_x_forwarded_for"]
      ["X-Forwarded-Proto" "$$scheme"]
      ["X-Forwarded-Host" "$$host"]
      ["X-Forwarded-Server" "$$host"]
    ];
  };
in
{
  options = {
    nixos.services.nginx = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
      };

      recommendedProxySettings = lib.mkOption {
        type = lib.types.bool;
        default = true;
      };

      proxyTimeout = lib.mkOption {
        type = lib.types.str;
        default = "60s";
        example = "20s";
        description = ''
          Change the proxy related timeouts in recommendedProxySettings.
        '';
      };

      virtualHosts = lib.mkOption {
        type = lib.types.attrsOf (
          lib.types.submodule {
            options = {
              locations = lib.mkOption {
                type = lib.types.attrsOf (
                  lib.types.submodule {
                    options.proxyPass = lib.mkOption {
                      type = lib.types.nullOr lib.types.str;
                      default = null;
                    };

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
      enable = extractWithPriority options [ "nixos" ] [ "services" "nginx" "enable" ];
      envsubst = extractWithPriority options [ "nixos" ] [ "services" "nginx" "enable" ];
      configuration = lib.mkIf config.nixos.services.nginx.enable (lib.singleton {
        daemon = "off";
        worker_processes = 8;
        user = "nginx";

        events."" = {
          use = "epoll";
          worker_connections = 512;
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

              # $connection_upgrade is used for websocket proxying
              map."$$http_upgrade $$connection_upgrade" = {
                default = "upgrade";
                "''"     = "close";
              };
            }
          ]
          ++ (lib.optionals cfg.recommendedProxySettings [
              {
                proxy_redirect          = "off";
                proxy_connect_timeout   = cfg.proxyTimeout;
                proxy_send_timeout      = cfg.proxyTimeout;
                proxy_read_timeout      = cfg.proxyTimeout;
                proxy_http_version      = "1.1";
                # don't let clients close the keep-alive connection to upstream. See the nginx blog for details:
                # https://www.nginx.com/blog/avoiding-top-10-nginx-configuration-mistakes/#no-keepalives
                proxy_set_header        = ["Connection" "''"];
              }
              recommendedProxyConfig
            ])
          ++ (lib.flip lib.mapAttrsToList cfg.virtualHosts (
            server_name: server: {
              server."" = {
                listen = [
                  "80"
                  "http2"
                ];
                inherit server_name;

                location = lib.flip lib.mapAttrs server.locations (
                  location: settings: [
                    (lib.optionalAttrs (settings.proxyPass != null && cfg.recommendedProxySettings)
                      recommendedProxyConfig)
                    (lib.optionalAttrs settings.proxyWebsockets {
                      proxy_http_version = "1.1";
                      proxy_set_header = [
                        [ "Upgrade""$$http_upgrade" ]
                        [ "Connection" "$$connection_upgrade" ]
                      ];
                    })
                    settings.extraConfig
                    (lib.optionalAttrs (settings.proxyPass != null) { proxy_pass = settings.proxyPass; })
                  ]
                );
              };
            }
          ));
      });
    };
  };
}
