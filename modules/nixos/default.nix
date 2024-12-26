{ lib, pkgs, ... }:
{
  imports = [
    ./systemd.nix
    ./nginx.nix
    ./users.nix
    ./postgresql.nix
    ./assertions.nix
    ./oauth2-proxy.nix
    ./nix.nix
    ./meta.nix
    ./networking.nix
    ./buildbot.nix
  ];

  options.nixos = lib.mkOption {
    type = lib.types.submodule {
      _module.args = {
        inherit pkgs;
      };
    };
    default = { };
  };
}
