{ lib, config, ... }:
{
  options = {
    nixos.services.postgresql = lib.mkOption {
      type = lib.types.unspecified;
      default = { };
    };
  };

  config = {
    services.postgresql = config.nixos.services.postgresql;
  };
}
