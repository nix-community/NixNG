{ pkgs, lib, config, ... }:
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
    script = pkgs.runCommandNoCCLocal "activation" (with pkgs; {
      nativeBuildInputs = [ pkgs.busybox ];
    })
      ''
        substituteAll ${pkgs.writeShellScript "activation" (builtins.readFile ./activation.sh)} $out
        chmod +x $out
      '';
  };
}
