{ pkgs, lib, nglib, config, ... }:
let
  cfg = config.services.attic;
  atticd = lib.getExe cfg.package;
  settingsFormat = pkgs.formats.toml { };

  validatedConfigFile = pkgs.stdenvNoCC.mkDerivation {
    name = "attic-validated.toml";
    src = cfg.configFile;
    dontUnpack = true;

    # Even the check-config mode needs a (dummy) secret.
    ATTIC_SERVER_TOKEN_HS256_SECRET_BASE64 = "kONlkVtBeH1PPoc7jLo0X3xKnNzuLhwYf030ghOTCH817P6jzqotxuhzRSrlOxS/VAmb5UEDobgw21EFGk8+XA==";

    buildPhase = ''
      ${atticd} --mode check-config --config $src
      cp $src $out
    '';
  };
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

        options = {
          storage = {
            type = lib.mkOption {
              type = lib.types.enum [ "local" "s3" ];
            };

            path = lib.mkOption {
              type = lib.types.str;
            };
          };
        };
      };
    };

    credentialsFile = lib.mkOption {
      description = ''
        File containing the server's secret token, generated using the command `openssl rand 64 | base64 -w0`.
      '';
      type = lib.types.str;
    };

    configFile = lib.mkOption {
      description = "Generated configuration file";
      type = lib.types.path;
      default = settingsFormat.generate "attic.toml" cfg.settings;
    };
  };

  config = lib.mkIf cfg.enable {
    init.services.attic = {
      enabled = true;

      ensureSomething.create.storageDir = lib.mkIf (cfg.settings.storage.type == "local") {
        type = "directory";
        mode = "0755";
        owner = "atticd:atticd";
        dst = cfg.settings.storage.path;
      };

      script = pkgs.writeShellScript "attic-run" ''
        export ATTIC_SERVER_TOKEN_HS256_SECRET_BASE64="$(<${cfg.credentialsFile})"
        chpst -b atticd ${atticd} --config ${validatedConfigFile}
      '';
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
