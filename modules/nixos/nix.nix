{ lib, config, ... }:
{
  options = {
    nixos.nix = {
      settings = lib.mkOption {
        type = lib.types.unspecified;
        default = { };
      };
    };
  };

  config = {
    nix.config = config.nixos.nix.settings;
  };
}
