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
  };

  config = lib.mkIf cfg.enable {
    init.services.jellyseerr = {
      enabled = true;
      workingDirectory = "${cfg.package}/libexec/jellyseerr/deps/jellyseerr";

      # TODO: simplify?
      script = pkgs.writeShellScript "jellyseerr-run.sh" ''
        ${lib.getExe cfg.package}
      '';
    };

    environment.systemPackages = [ cfg.package ];
  };
}
