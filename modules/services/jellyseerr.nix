{
  lib,
  pkgs,
  config,
  ...
}:
let
  cfg = config.services.jellyseerr;
in
{
  options.services.jellyseerr = {
    enable = lib.mkEnableOption "jellyseerr";
    package = lib.mkPackageOption pkgs "jellyseerr" { };

    port = lib.mkOption {
      description = ''
        The port Jellyseerr should listen on.
      '';
      type = lib.types.port;
      example = 8080;
      default = 5055;
    };

    configDir = lib.mkOption {
      description = ''
        The directory to save run-time configuration.
      '';
      type = lib.types.str;
      example = "/jellyseerr";
      default = "/var/lib/jellyseerr";
    };
  };

  config = lib.mkIf cfg.enable {
    init.services.jellyseerr = {
      enabled = true;
      workingDirectory = "${cfg.package}/libexec/jellyseerr/deps/jellyseerr";
      script = lib.getExe cfg.package;
    };

    environment = {
      systemPackages = [ cfg.package ];
      variables = {
        PORT = builtins.toString cfg.port;
        CONFIG_DIRECTORY = cfg.configDir;
      };
    };
  };
}
