{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.system;
in
{
  options.system = {
    activation = mkOption {
      description = ''
        A set of shell script fragments that are executed when a NixNG system
        configuration is activated. You can update /etc, 
        create accounts, and so on. For creating service related directories or file,
        please user <option>init.services.<service>.ensureSomething</option>.
        These fragments are isolated and can't effect each other environment.
        These are run every time the system configuration is activated, which alse
        happens at boot, therefore it's important that these scripts are idempotent
        and fast.
      '';
      type = types.attrsOf types.str;
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
    system.activationScript = pkgs.writeShellScript "activation"
      ''
        ${concatStringsSep "\n" (mapAttrsToList (n: v:
          ''
            echo "Running activation script ${n}"
            (
              ${v}
            ) || echo "Activation script ${n} exited with code $!"
          ''
        ) cfg.activation)}
      '';
  };
}
