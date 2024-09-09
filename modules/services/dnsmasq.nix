{ pkgs, lib, nglib, config, ... }:
with lib; with nglib;
let
  cfg = config.services.dnsmasq;
in
{
  options.services.dnsmasq = {
    enable = mkEnableOption "dnsmasq";
    package = mkPackageOption pkgs "dnsmasq" { };

    user = mkOption {
      description = "dnsmasq user";
      type = types.str;
      default = "dnsmasq";
    };

    group = mkOption {
      description = "dnsmasq group";
      type = types.str;
      default = "dnsmasq";
    };

    configuration = mkOption {
      type = types.str;
    };
  };

  config = mkIf cfg.enable (
    let
      configFile = builtins.toFile "dnsmasq.conf" cfg.configuration;
    in
    {
      init.services.dnsmasq = {
        enabled = true;

        # This is the default directory for dnsmasq's leasefile.
        ensureSomething.create."/var/lib/misc" = {
          type = "directory";
          mode = "755";
          owner = "root:root";
          persistent = true;
          dst = "/var/lib/misc";
        };

        script = pkgs.writeShellScript "dnsmasq-run" ''
          chpst -u ${cfg.user}:${cfg.group} -b dnsmasq ${lib.getExe cfg.package} \
            --keep-in-foreground \
            --pid-file=/run/dnsmasq.pid \
            --conf-file=${configFile} \
            --user=${cfg.user} \
            --group=${cfg.group}
        '';
      };

      environment.systemPackages = [ cfg.package configFile ];

      users.users.${cfg.user} = mkDefaultRec {
        description = "dnsmasq";
        group = cfg.group;
        createHome = false;
        home = "/var/empty";
        useDefaultShell = true;
        uid = config.ids.uids.dnsmasq;
      };

      users.groups.${cfg.group} = mkDefaultRec {
        gid = config.ids.gids.dnsmasq;
      };
    }
  );
}
