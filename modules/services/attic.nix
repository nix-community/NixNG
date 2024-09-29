{ pkgs, lib, nglib, config, ... }:
let
  cfg = config.services.attic;
  settingsFormat = pkgs.formats.toml { };
in
{
  options.services.attic = {
    enable = lib.mkEnableOption "attic";
    package = lib.mkPackageOption pkgs "attic-server" { };

    user = lib.mkOption {
      description = "attic user";
      type = lib.types.str;
      default = "atticd";
    };

    group = lib.mkOption {
      description = "attic group";
      type = lib.types.str;
      default = "atticd";
    };

    settings = lib.mkOption {
      type = lib.types.submodule {
        freeformType = settingsFormat.type;
      };
    };

    credentialsFile = lib.mkOption {
      description = ''
        File containing the server's secret token, generated using the command `openssl rand 64 | base64 -w0`.
      '';
      type = lib.types.str;
    };
  };

  config = lib.mkIf cfg.enable {
    init.services.attic = {
      enabled = true;

      script = pkgs.writeShellScript "attic-run"
        (
          let
            configFile = settingsFormat.generate "attic.toml" cfg.settings;
          in
          ''
            export ATTIC_SERVER_TOKEN_HS256_SECRET_BASE64="$(<${cfg.credentialsFile})"
            ${cfg.package}/bin/atticadm --config ${configFile} make-token --sub demo --validity '1 day' --pull '*' --push '*' --create-cache '*'
            chpst -b atticd ${lib.getExe cfg.package} \
              --config ${configFile}
          ''
        );
    };

    environment.systemPackages = [ cfg.package ];

    users.users.${cfg.user} = nglib.mkDefaultRec {
      description = "atticd";
      group = cfg.group;
      createHome = false;
      home = "/var/empty";
      useDefaultShell = true;
      uid = config.ids.uids.attic;
    };

    users.groups.${cfg.group} = nglib.mkDefaultRec {
      gid = config.ids.gids.attic;
    };
  };
}
