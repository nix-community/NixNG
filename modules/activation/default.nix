{ pkgs, lib, config, nglib, ... }:
with lib;
let
  cfg = config.activation;
in
{
  options.activation = {
    enable = mkOption {
      default = true;
      description = "Enable activation script.";
      type = types.bool;
    };
    script = mkOption {
      description = "Activation script.";
      type = types.path;
      readOnly = true;
    };
  };

  config.activation = mkIf cfg.enable {
    script = nglib.writeSubstitutedShellScript {
      name = "activation";
      file = ./activation.sh;
      substitutes = {};
    };
  };
}
