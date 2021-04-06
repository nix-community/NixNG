{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.socklog;
in
{
  options.services.socklog = {
    enable = mkEnableOption "Enable socklog.";

    unix = mkOption {
      description = "Make socklog listen on a unix domain socket. Input the path to the UDS socklog should use.";
      type = with types; nullOr path;
      default = null;
    };
    inet = mkOption {
      description = "Make socklog listen on UDP.";
      type = with types; nullOr (submodule {
        options = {
          ip = mkOption {
            description = ''
              The IP address on which to listen on, must be an interface
              or 0 for all. Doesn't accept `localhost`.
'';
            type = types.str;
            default = "127.0.0.1";
          };
          port = mkOption {
            description = "The port on which to listen on.";
            type = types.port;
            default = 514;
          };
        };
      });
      default = null;
    };
  };
  config = mkIf cfg.enable {
    init.services.socklog =
      let
        unixSocklog =
          optionalString (cfg.unix != null) "socklog unix ${cfg.unix} &";
        inetSocklog =
          optionalString (cfg.inet != null) "socklog inter ${cfg.inet.ip} ${toString cfg.inet.port} &";
      in
        {
          script = pkgs.writeShellScript "socklog-run" ''
            set -m

            export PATH=${pkgs.socklog}/bin:$PATH
            
            trap 'kill %1; kill %2' SIGINT SIGTERM
            ${unixSocklog}
            ${inetSocklog}
            fg
          '';
        };

    assertions = [
      {
        assertion = !(cfg.unix == null && cfg.inet == null);
        message = "You must select at least one listening endpoint for socklog, either `unix` or `inet`";
      }
    ];
  };
}
