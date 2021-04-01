{ pkgs, lib, config, nglib, ... }:
with lib;
let
  cfg = config.runit;
  cfgInit = config.init;
in
{
  options.runit = {
    enable = mkEnableOption "Enable runit";
    runtimeServiceDirectory = mkOption {
      description = "where runsvdir should create superwise and log directories for services";
      type = types.path;
      default = "/run/sv";
    };
    stages = mkOption {
      description = "runit stages";
      default = {};
      type = types.submodule {
        options = {
          stage-1 = mkOption {
            type = types.path;
            description = "runit stage 1";
            default = nglib.writeSubstitutedShellScript {
              name = "1";
              file = ./stage-1.sh;
              substitutes = {};
            };
          };
          stage-2 = mkOption {
            type = types.path;
            description = "runit stage 2";
            default = nglib.writeSubstitutedShellScript {
              name = "2";
              file = ./stage-2.sh;
              substitutes = {
                inherit (pkgs) runit findutils busybox;
                inherit (cfg) serviceDir runtimeServiceDirectory;
              };
            };
          };
          stage-3 = mkOption {
            type = types.path;
            description = "runit stage 3";
            default = nglib.writeSubstitutedShellScript {
              name = "3";
              file = ./stage-3.sh;
              substitutes = {};
            };
          };
        };
      };
    };
    serviceDir = mkOption {
      description = "Generated service directory";
      type = types.path;
      readOnly = true;
    };
  };

  config = {
    runit = let
      serviceDir = pkgs.runCommandNoCCLocal "service-dir" {} ''
        mkdir $out
        ${lib.concatStringsSep "\n" (mapAttrsToList (name: service:
          assert service.dependencies == [];
          ''
            mkdir $out/${name}
            ln -s ${service.script} $out/${name}/run
          ''
        ) cfgInit.services)}
      '';
    in
      {
        inherit serviceDir;
      };

    init = mkMerge [
      {
        availableInits = [ "runit" ];
      }
      (mkIf cfg.enable {
        type = "runit";
        script = pkgs.writeShellScript "init"
          ''
          export PATH=${pkgs.busybox}/bin
          mkdir -p /etc/runit

          ln -sf ${cfg.stages.stage-1} /etc/runit/1
          ln -sf ${cfg.stages.stage-2} /etc/runit/2
          ln -sf ${cfg.stages.stage-3} /etc/runit/3

          exec ${pkgs.runit}/bin/runit
        '';
      })
    ];
  };
}
