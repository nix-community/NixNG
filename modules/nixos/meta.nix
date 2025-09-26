{ lib, ... }:
{
  options = {
    nixos.meta = lib.mkOption { type = lib.types.unspecified; };
  };
}
