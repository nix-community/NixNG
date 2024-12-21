{
  pkgs,
  lib,
  config,
  nglib,
  ...
}:
let
  cfg = config.services.ntfy-sh;
  cfgInit = config.init.services.ntfy-sh;
  ntfy-sh = "${cfg.package}/bin/ntfy";
  settingsFormat = pkgs.formats.yaml { };
in
{
  options.services.ntfy-sh = {
    enable = lib.mkEnableOption "ntfy-sh";
    package = lib.mkPackageOption pkgs "ntfy-sh" { };

    settings = lib.mkOption {
      type = lib.types.submodule {
        freeformType = settingsFormat.type;

        options = {
          base-url = lib.mkOption {
            type = lib.types.str;
            example = "https://ntfy.example.com";

            description = ''
              Externally visible base URL for this host.
            '';
          };
        };
      };

      description = ''
        Configuration for ntfy.sh.
        Supported values can be found [here](https://ntfy.sh/docs/config/#config-options).
      '';
    };

    configFile = lib.mkOption {
      description = "Generated configuration file";
      type = lib.types.path;
      default = settingsFormat.generate "server.yml" cfg.settings;
    };
  };

  config = lib.mkIf cfg.enable {
    init.services.ntfy-sh = {
      enabled = true;
      user = lib.mkDefault "ntfy-sh";
      group = lib.mkDefault "ntfy-sh";

      script = pkgs.writeShellScript "ntfy-sh-run" ''
        ${ntfy-sh} serve --config ${cfg.configFile}
      '';
    };

    environment.systemPackages = [ cfg.package ];

    users.users.${cfgInit.user} = nglib.mkDefaultRec {
      description = "ntfy-sh";
      group = cfgInit.group;
      createHome = false;
      home = "/var/empty";
      useDefaultShell = true;
      uid = config.ids.uids.ntfy-sh;
    };

    users.groups.${cfgInit.group} = nglib.mkDefaultRec { gid = config.ids.gids.ntfy-sh; };
  };
}
