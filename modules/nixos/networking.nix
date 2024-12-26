{ lib, config, ... }:
{
  options = {
    nixos.networking.hostName = lib.mkOption { type = lib.types.str; };
  };

  config = {
    nixos.networking.hostName = "buildbot";
  };
}
