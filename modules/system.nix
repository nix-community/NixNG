{ pkgs, lib, nglib, config, ... }:
with lib;
let
  cfg = config.system;

  configFinal = config;
in
{
  options.system = {
    createEnvSh = mkOption {
      description = ''
        Automatically create /bin/sh and /usr/bin/env, without the former
        even some system calls will fails, most notable PostreSQL will exit
        with a cryptic error.
      '';
      type = types.bool;
      default = true;
    };

    build = {
      toplevel = mkOption {
        description = ''
          The full system, built up.
        '';
        type = types.path;
      };

      ociImage = mkOption {
        description = ''
          OCI compatible image.
        '';
        type = types.submodule {
          options = {
            build = mkOption {
              description = ''
                A path to a OCI image in a gziped tarball.
              '';
              type = types.path;
            };
            stream = mkOption {
              description = ''
                A script which builds an OCI image and outputs what it builds
                into stdout without saving to disk.
              '';
              type = types.path;
            };
          };
        };
      };
    };

    activation = mkOption {
      description = ''
        A set of shell script fragments that are executed when a NixNG system
        configuration is activated. You can update /etc, 
        create accounts, and so on. For creating service related directories or file,
        please use <option>init.services.<service>.ensureSomething</option>.
        These fragments are isolated and can't effect each other's environment.
        These are run every time the system configuration is activated, which also
        happens at boot, therefore it's important that these scripts are idempotent
        and fast.
      '';
      type = with types; attrsOf (submodule {
        options = {
          data = mkOption {
            description = ''
              Script fragment which to run.
            '';
            type = types.str;
          };
          before = mkOption {
            description = ''
              Script before dependencies. See <literal>/lib/dag.nix</literal>.
            '';
            type = with types; listOf str;
          };
          after = mkOption {
            description = ''
              Script after dependencies. See <literal>/lib/dag.nix</literal>
            '';
            type = with types; listOf str;
          };
        };
      });
      apply = x: nglib.dag.dagTopoSort x;
      default = {};
    };
    activationScript = mkOption {
      description = ''
        Script generated from <option>system.activation</option>, used to setup the environment. 
      '';
      type = types.path;
      readOnly = true;
    };
  };

  config = {
    system.build = {
      toplevel = pkgs.runCommandNoCC "nixng"
        { nativeBuildInputs = with pkgs; [ busybox ]; }
        (with configFinal;
          ''
          mkdir $out
          ln -s ${init.script} $out/init
          ln -s ${system.activationScript} $out/activation
        '');

      ociImage =
        let
          config = {
            name = "nixng";
            tag = "latest";

            config = {
              StopSignal = "SIGCONT";
              Entrypoint =
                [ "${configFinal.init.script}"
                ];
            };
          };
        in
          with pkgs; {
            build = dockerTools.buildLayeredImage config;
            stream = dockerTools.streamLayeredImage config;
          };
    };

    
    system.activation.createEnvSh = mkIf cfg.createEnvSh
      (nglib.dag.dagEntryAnywhere
        ''
        # Borrowed from NixOS therefore it's licensed under the MIT license
        #### Activation script snippet usrbinenv:
        _localstatus=0
        mkdir -m 0755 -p /usr/bin
        ln -sfn ${pkgs.busybox}/bin/env /usr/bin/.env.tmp
        mv /usr/bin/.env.tmp /usr/bin/env # atomically replace /usr/bin/env

        # Create the required /bin/sh symlink; otherwise lots of things
        # (notably the system() function) won't work.
        mkdir -m 0755 -p /bin
        ln -sfn "${pkgs.busybox}/bin/sh" /bin/.sh.tmp
        mv /bin/.sh.tmp /bin/sh # atomically replace /bin/sh
      '');

    system.activationScript = pkgs.writeShellScript "activation"
      ''
        _status=0
        trap "_status=1 _localstatus=\$?" ERR

        ${concatStringsSep "\n" (map (dag:
          ''
            _localstatus=0
            echo "Running activation script ${dag.name}"
            (
              ${dag.data}
            )
            if (( _localstatus > 0 )); then
              printf "Activation script snippet '%s' failed (%s)\n" "${dag.name}" "$_localstatus"
            fi
          ''
        ) cfg.activation.result)}
        
        exit $_status
      '';

    assertions = [
      {
        assertion = !(cfg.activation ? "cycle" || cfg.activation ? "loops");
        message = ''
          `cfg.activation` has one or more cycles and/or loops.
          - cycles: 
            ${(map (x: "{ after = [ ${concatSepStrings " " x.after} ]; data = ${x.data}; name = ${x.name} }") cfg.activation.loops or []) or ""}
          - loops:
            ${(map (x: "{ after = [ ${concatSepStrings " " x.after} ]; data = ${x.data}; name = ${x.name} }") cfg.activation.loops or [])}
        '';
      }
    ];
  };
}
