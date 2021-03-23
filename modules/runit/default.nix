{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.runit;
in
{
  options.runit = {
    enable = mkEnableOption "Enable runit";
    stages = mkOption {
      description = "runit stages";
      default = {};
      type = types.submodule {
        options = {
          stage-1 = mkOption {
            type = types.path;
            description = "runit stage 1";
            default = pkgs.writeShellScript "1"
              ''
                ${builtins.readFile ./stage-1.sh}
              '';
          };
          stage-2 = mkOption {
            type = types.path;
            description = "runit stage 2";
            default = pkgs.writeShellScript "2"
              ''
                ${builtins.readFile ./stage-2.sh}
              '';
          };
          stage-3 = mkOption {
            type = types.path;
            description = "runit stage 3";
            default = pkgs.writeShellScript "3"
              ''
                ${builtins.readFile ./stage-3.sh}
              '';
          };
        };
      };
    };
  };

  config.init = mkMerge [
    {
      availableInits = [ "runit" ];
    }
    (mkIf cfg.enable {
      type = "runit";
      script = pkgs.writeShellScript "init"
        ''
          mkdir -p /etc/runit

          ln -sf ${cfg.stages.stage-1} /etc/runit/1
          ln -sf ${cfg.stages.stage-2} /etc/runit/2
          ln -sf ${cfg.stages.stage-3} /etc/runit/3
        '';
    })
  ];
}
