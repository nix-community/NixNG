{ config, nglib, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.crond;
in
{
  options.services.crond = {
    enable = mkEnableOption "Enable crond, the daemon to execute scheduled commands, specifically cronie.";
    package = mkOption {
      type = types.package;
      description = "The package which contains a cron implementation.";
      default = pkgs.cronie;
    };

    crontabs = mkOption {
      type = with types; attrsOf (submodule {
        options = {
          environment = mkOption {
            type = attrsOf str;
            description = ''
              A set of environment variable to set for this specific crontab.
            '';
            example = ''
              {
                MAILTO="root";
              }
            '';
            default = {};
          };
          jobs = mkOption {
            type = listOf str;
            description = ''
              A list of job entries, in the usual cron format.
            '';
            example = ''
              [
                "5 0 * * * www-data rm /var/log/{httpd.access,httpd.error}"
              ]
            '';
            default = [];
          };
        };
      });
      description = ''
        Defines cron jobs, allows for the creation of multiple files and entries.
      '';
      default = {};
      example = ''
        {
          "delete-log" = {
            environment = {
              MAILTO="root";
            };
            jobs = [
              "5 0 * * * www-data rm /var/log/{httpd.access,httpd.error}"
            ];
          };
        }
      '';

      apply = x:
        let
          cronfiles =
            (mapAttrsToList
              (n: v: pkgs.writeText n
                ''
                  ${concatStringsSep "\n" (mapAttrsToList (n: v: ''"${n}"="${v}"'') v.environment)}
                  ${concatStringsSep "\n" v.jobs}
                '')
              x);
        in
          pkgs.runCommandNoCCLocal "cron.d" {} ''
            CRONFILES="${concatStringsSep " " cronfiles}"
            mkdir -p $out
            for cronfile in "$CRONFILES" ; do
              ln -s "$cronfile" $out/$(basename "$cronfile")
            done
          '';
    };
  };
  
  config = mkIf cfg.enable {
    system.activation."crond" = nglib.dag.dagEntryAnywhere
      ''
        mkdir -p /etc /var/run /var/spool/cron /var/cron
        ln -s ${cfg.crontabs}/ /etc/cron.d
        ls -lahR /etc/cron.d/
      '';

    init.services.crond =
      {
        script = ''
          ${cfg.package}/bin/crond -n -x ext,sch,misc
        '';
      };
  };
}
