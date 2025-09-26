{
  lib,
  pkgs,
  config,
  __enableExperimentalNixOSCompatibility,
  ...
}:
{
  imports = lib.optionals __enableExperimentalNixOSCompatibility ([
    ./systemd.nix
    ./buildbot.nix
    ./nginx.nix
    ./users.nix
    ./postgresql.nix
    ./assertions.nix
    ./oauth2-proxy.nix
    ./nix.nix
    ./meta.nix
    ./networking.nix
  ]);

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

  config.assertions = lib.optionals __enableExperimentalNixOSCompatibility [
    {
      assertion = (config.nixos.acceptRisks == "I accept the risks");
      message = ''
        NixOS module compatibility is highly experimental, severely unfinished and most definitely has
        functional and security bugs. Unless you know what you're doing and are willing to accept the risks
        reconsider it's usage. To signify you are aware of these risks, set the option
        `config.nixos.acceptRisks` to `"I accept the risks"`.

        If you run into any of the aforementioned deficiencies please reach out on Matrix at
        `#nixng:matrix.redalder.org`.
      '';
    }
  ];
}
