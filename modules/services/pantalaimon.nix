{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.pantalaimon;

  dataDir = "/var/lib/pantalaimon";
in
{
  options.services.pantalaimon = {
    enable = mkEnableOption "Enable pantalaimon";

    package = mkOption {
      description = "Pantalaimon package to use";
      default = pkgs.pantalaimon.override { enableDbusUi = false; };
      type = types.package;
    };

    config = mkOption {
      description = ''
        Pantalaimon configuration file in Nix form. <link>https://github.com/matrix-org/pantalaimon/blob/master/docs/man/pantalaimon.5.md</link>.
      '';
      example = literalExample ''
        {
          Default =
            {
              LogLevel = "Debug";
              SSL = "True";
              Notifications = "On";
            };

          Clocktown =
            {
              Homeserver = "https://localhost:8448";
              ListenAddress = "localhost"
              ListenPort = 8009
              Proxy = "http://localhost:8080";
              SSL = "False";
            };
        }
      '';
      type = with types; attrsOf (attrsOf (oneOf [ string int ]));
      apply = x: with pkgs; writeText "pantalaimon.conf" (generators.toINI {} x);
    };

    user = mkOption {
      description = "User to run Pantalaimon under.";
      default = "pantalaimon";
      type = types.str;
    };

    group = mkOption {
      description = "Group to run Pantalaimon under.";
      default = "pantalaimon";
      type = types.str;
    };
  };

  config = mkIf cfg.enable {
    users.users.${cfg.user} = mapAttrs (_: mkDefault) {
      description = "Pantalaimon";
      group = cfg.group;
      home = "/var/empty";
      createHome = false;
      uid = config.ids.uids.pantalaimon;
    };

    users.groups.${cfg.group} = {
      gid = mkDefault config.ids.gids.pantalaimon;
    };

    init.services.pantalaimon =
      {
        ensureSomething.create."dataDir" = {
          type = "directory";
          mode = "770";
          owner = "${cfg.user}:${cfg.group}";
          dst = dataDir;
          persistent = true;
        };

        script = pkgs.writeShellScript "pantalaimon-run"
          ''
            echo AAAA
            ${cfg.package}/bin/pantalaimon -c ${cfg.config}
          '';
        enabled = true;
      };
  };
}
