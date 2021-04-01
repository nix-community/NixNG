{ pkgs, lib, config, nglib, ... }:
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
      script = nglib.writeSubstitutedShellScript {
        name = "init";
        file = ./init.sh;
        substitutes = with pkgs; {
          inherit eudev bash busybox;
        };
      };
    })
  ];
}
