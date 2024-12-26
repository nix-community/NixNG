{ config, lib, ... }:
{
  options.nixos = lib.mkOption { type = lib.types.submodule { imports = [ ../assertions.nix ]; }; };

  config.assertions = config.nixos.assertions;
}
