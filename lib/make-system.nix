{ pkgs, callPackage, system, dockerTools
, runCommandNoCC, lib, nglib
, busybox, config, name
}:

let
  defaultModules = [
    ../modules/runit
    ../modules/dumb-init
    ../modules/initrd
    ../modules/initramfs
    ../modules/init.nix
    ../modules/system.nix
    ../modules/assertions.nix
    ../modules/bootloader
    ../modules/nix.nix

    ../modules/security/ca.nix

    ../modules/environment.nix
    ../modules/users.nix
    ../modules/ids.nix

    ../modules/services/apache2-nixos.nix
    ../modules/services/gitea.nix
    ../modules/services/getty.nix
    ../modules/services/socklog.nix
    ../modules/services/crond.nix
  ];

  evaledModules = lib.evalModules
    { 
      modules = defaultModules ++ [ config ({ ... }:
        {
          _module.args = {
            inherit pkgs system nglib;
          };
        }
      )];
    };

  failedAssertions = map (x: x.message) (lib.filter (x: !x.assertion) evaledModules.config.assertions);
  configValid =
    if failedAssertions != [] then
      throw "\nFailed assertions:\n${lib.concatStringsSep "\n" (map (x: "- ${x}") failedAssertions)}"
    else
      evaledModules.config;
in 
evaledModules // { config = configValid; }
