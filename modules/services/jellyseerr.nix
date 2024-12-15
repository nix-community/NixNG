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
  };

  config = lib.mkIf cfg.enable {
    init.services.jellyseerr = {
      enabled = true;
      workingDirectory = "${cfg.package}/libexec/jellyseerr/deps/jellyseerr";
      script = lib.getExe cfg.package;
    };

    environment = {
      systemPackages = [ cfg.package ];
      variables.PORT = builtins.toString cfg.port;
    };
  };
}
