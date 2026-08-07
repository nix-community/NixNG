{ lib, ... }:
{
  options = {
    nixos.services.oauth2-proxy = lib.mkOption {
      type = lib.types.unspecified;
      default = { };
      description = ''
        WARNING: this is only a stub module that does nothing.
      '';
    };
  };
}
