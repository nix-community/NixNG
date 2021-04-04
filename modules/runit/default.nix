{ pkgs, lib, config, nglib, ... }:
with lib;
let
  cfg = config.runit;
  cfgInit = config.init;
in
{
  options.runit = {
    enable = mkEnableOption "Enable runit";

    pkg = mkOption {
      description = "runit package to use";
      type = types.package;
      default = pkgs.runit;
    };
    isContainer = mkEnableOption "whether runit should shutdown just to a SIGCONT";

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
              substitutes = {
                inherit (cfg) isContainer;
              };
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
    runit = 
      {
        serviceDir = pkgs.runCommandNoCCLocal "service-dir" {} ''
          mkdir $out
          ${concatStringsSep "\n" (mapAttrsToList (n: s:
            let
              run = pkgs.writeShellScript "${n}-run" ''
                ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
                  with cv;
                  ''
                    if ! [[ -e ${dst} ]] ; then
                      echo '${n}: linking `${src}` to `${dst}`'
                      mkdir -p "$(dirname '${dst}')"
                      ln -s '${src}' '${dst}'
                    fi
                  ''
                ) s.ensureSomething.link)}  

                ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
                  with cv;
                  ''
                    if ! [[ -e ${dst} ]] ; then
                      echo '${n}: copying `${src}` to `${dst}`'
                      mkdir -p "$(dirname '${dst}')"
                      cp -r '${src}' '${dst}'
                    fi
                  ''
                ) s.ensureSomething.copy)}  

                ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
                  abort "linkFarm is not implemented yet in runit!"
                ) s.ensureSomething.linkFarm)}  

                ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
                  with cv;
                  ''
                    if ! [[ -e ${dst} ]] ; then
                      echo '${n}: executing `${executable}` to create `${dst}`'
                      mkdir -p "$(dirname '${dst}')"
                      out=${dst} ${executable}
                      
                      if ! [[ -e ${dst} ]] ; then
                        echo '${n}: executed `${executable}` but `${dst}`
                    fi
                  ''
                ) s.ensureSomething.exec)}  

                ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
                  with cv;
                  ''
                    if ! [[ -e ${dst} ]] ; then
                      echo '${n}: creating `${dst}`'

                      ${if (type == "directory") then
                          "mkdir -p ${dst}"
                        else if (type == "file") then
                          ''
                            mkdir -p "$(dirname '${dst}')"
                            touch ${dst}
                          ''
                        else
                          abort "Unsupported init create type, module system should have caught this!"
                       } 
                      
                      chown ${owner} ${dst}
                      ${optionalString (mode != null) "chmod ${mode} ${dst}"}
                    fi
                  ''
                ) s.ensureSomething.create)}
 
                exec ${s.script}
              '';
            in

            assert s.dependencies == [];

            ''
              mkdir $out/${n}
              ln -s ${run} $out/${n}/run
            ''
          ) cfgInit.services)}
        '';
      };

    init = mkMerge [
      {
        availableInits = [ "runit" ];
      }
      (mkIf cfg.enable {
        type = "runit";
        script = pkgs.writeShellScript "init"
          ''
          export PATH=${pkgs.busybox}/bin:${cfg.pkg}/bin
          mkdir -p /etc/runit

          ln -sf ${cfg.stages.stage-1} /etc/runit/1
          ln -sf ${cfg.stages.stage-2} /etc/runit/2
          ln -sf ${cfg.stages.stage-3} /etc/runit/3

          exec runit-init
        '';
      })
    ];
  };
}
