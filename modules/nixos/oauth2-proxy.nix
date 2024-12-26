{ lib, ... }:
{
  options = {
    nixos.services.oauth2-proxy = lib.mkOption {
      type = lib.types.unspecified;
      default = { };
    };
  };
}
