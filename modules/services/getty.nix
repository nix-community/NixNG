{ pkgs, config, lib, ... }:
with lib;
let
  cfg = config.services.getty;
in
{
  options.services.getty = mkOption {
    description = "All of the agettys";
    type = types.attrsOf (types.submodule {
      options = {
        baudRate = mkOption {
          description = "TTY baud rate";
          type = types.int;
        };
        termName = mkOption {
          description = "TTY terminal name";
          type = types.str;
          default = "vt100";
        };
        assume8BitTty = mkOption {
          description = "Whether to assume the tty is 8-bit";
          type = types.bool;
          default = true;
        };
        # TODO Local line -L
        pkg = mkOption {
          description = "getty package";
          type = types.path;
          default = "${pkgs.utillinuxMinimal}/bin/agetty";
        };
      };
    });
    default = {};
  };
  config.init.services = mapAttrs'
    (name: getty: nameValuePair "getty-${name}"
      {
        script = with getty;
          pkgs.writeShellScript "getty-${name}-run"
          ''
            exec setsid ${pkg} ${optionalString assume8BitTty "-8"} "${name}" "${toString baudRate}" "${termName}"
          '';
      })
    cfg;
}
