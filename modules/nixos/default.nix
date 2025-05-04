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
  ];

  options.nixos = lib.mkOption {
    type = lib.types.submodule {
      options = {
        acceptRisks = lib.mkOption {
          visible = false;
          description = ''
            This is an invisible option, intention is for the user to hit the scary warning first
            and only then learn of this option and declare acceptance of the risks.
          '';
          type = lib.types.str;
          default = "I don't know of the risks";
        };
      };
      config._module.args = {
        inherit pkgs;
      };
    };
    default = { };
  };
}
