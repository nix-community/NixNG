{ pkgs, lib, config, nglib, ... }:
with lib;
let
  cfg = config.environment;
  profileScript = pkgs.writeShellScript "profile" cfg.shell.profile;
in
{
  options.environment = {
    variables = mkOption {
      default = {};
      example = { EDITOR = "vim"; BROWSER = "firefox"; };
      type = with types; attrsOf (either str (listOf str));
      apply = x: concatStringsSep ":"
        (mapAttrsToList (n: v: "${n}=" + (if isList v then concatStringsSep ":" v else v)) x);
    };

    shell = {
      profile = mkOption {
        description = ''
          Shell script fragments, concataned into /etc/profile.
        '';
        type = with types; listOf str;
        apply = x: concatStringsSep "\n" x;
        default = [];
      };
    };
  };

  config = {
    environment.shell.profile =[
      ''
        export ${cfg.variables}
      ''
    ];

    system.activation.shellProfile = nglib.dag.dagEntryAnywhere ''
      export PATH=${pkgs.busybox}/bin

      mkdir -m 0755 -p /etc
      ln -sfn ${profileScript} /etc/.profile.tmp 
      mv /etc/.profile.tmp /etc/profile # atomically replace /etc/profile
    '';
  };
}
