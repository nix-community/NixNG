{ lib, config, pkgs, ... }:
with lib;
let
  cfg = config.services.apache2.premade.basic;
in
{
  options.services.apache2.premade.basic = {
    enable = mkEnableOption "Enable basic configuration";
    logFormat = mkOption {
      type = types.str;
      default = "common";
      example = "combined";
      description = ''
        Log format for log files. Possible values are: combined, common, referer, agent.
        See <link xlink:href="https://httpd.apache.org/docs/2.4/logs.html"/> for more details.
      '';
    };

    logDir = mkOption {
      type = types.path;
      default = "/var/log/httpd";
      description = ''
        Directory for Apache's log files. It is created automatically.
      '';
    };
  };

  config.services.apache2 = mkIf cfg.enable {
    configuration =
      (if cfg.logFormat != "none" then
        {
          ErrorLog = "${cfg.logDir}/error.log";
          LogLevel = "notice";
          LogFormat = [
            [ "%h %l %u %t \"%r\" %>s %b \"%{Referer}i\" \"%{User-Agent}i\"" "combined" ]
            [ "%h %l %u %t \"%r\" %>s %b" "common" ]
            [ "%{Referer}i -> %U" "referer" ]
            [ "%{User-agent}i" "agent" ]
          ];
          CustomLog = [ "${cfg.logDir}/access.log" "${cfg.logFormat}" ];
        }
       else
         {
           ErrorLog = "/dev/null";
         });
  };
}
