{
  lib,
  pkgs,
  config,
  nglib,
  ...
}:
let
  cfg = config.services.radicale;

  settingsFormat = pkgs.formats.ini {
    listToValue = lib.concatMapStringsSep ", " (lib.generators.mkValueStringDefault { });
  };
in
{
  options.services.radicale = {
    enable = lib.mkEnableOption "radicale";
    package = lib.mkPackageOption pkgs "radicale" { };

    user = lib.mkOption {
      description = "radicale user";
      type = lib.types.str;
      default = "radicale";
    };

    group = lib.mkOption {
      description = "radicale group";
      type = lib.types.str;
      default = "radicale";
    };

    settings = lib.mkOption {
      type = settingsFormat.type;
      default = { };
      description = ''
        Configuration for Radicale. See
        <https://radicale.org/v3.html#configuration>.
      '';
      example = lib.literalExpression ''
        server = {
          hosts = [ "0.0.0.0:5232" "[::]:5232" ];
        };
        auth = {
          type = "htpasswd";
          htpasswd_filename = "/etc/radicale/users";
          htpasswd_encryption = "bcrypt";
        };
        storage = {
          filesystem_folder = "/var/lib/radicale/collections";
        };
      '';
    };
  };

  config = lib.mkIf cfg.enable (
    let
      configFile = settingsFormat.generate "radicale.ini" cfg.settings;
    in
    {
      init.services.radicale = {
        enabled = true;

        script = pkgs.writeShellScript "radicale-run" ''
          chpst -u ${cfg.user}:${cfg.group} ${cfg.package}/bin/radicale \
            --config ${configFile}
        '';
      };

      environment.systemPackages = [ cfg.package ];

      users.users.${cfg.user} = nglib.mkDefaultRec {
        description = "radicale";
        group = cfg.group;
        createHome = false;
        home = "/var/empty";
        useDefaultShell = true;
        uid = config.ids.uids.radicale;
      };

      users.groups.${cfg.group} = nglib.mkDefaultRec { gid = config.ids.gids.radicale; };
    }
  );
}
