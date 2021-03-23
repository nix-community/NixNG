{ pkgs, lib, config, ... }:
with lib;
let
  cfg = config.initrd;
in
{
  options.initrd = {
    enable = mkEnableOption "Enable initrd as init";
  };

  config.init = mkMerge [
    {
      availableInits = [ "initrd" ];
    }
    (mkIf cfg.enable {
      type = "initrd";
      script = pkgs.runCommandNoCCLocal "init" (with pkgs; {
        inherit eudev bash busybox;
        nativeBuildInputs = [ pkgs.busybox ];
      })
        ''
          substituteAll ${pkgs.writeShellScript "init" (builtins.readFile ./init.sh)} $out
          chmod +x $out
        '';
    })
  ];
}
