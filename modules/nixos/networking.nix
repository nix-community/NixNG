{ lib, config, ... }:
{
  options = {
    nixos.networking.hostName = lib.mkOption {
      type = lib.types.str;
      description = ''
        Machine hostname, this has currently no effect on NixNG and is completely
        local to the NixOS compatibility layer.
      '';
      default = "unnamed";
    };
  };
}
