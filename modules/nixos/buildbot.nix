{ lib, nglib, nixngInputs, ... }:
let
  buildbotMasterModule =
    nixngInputs.nixpkgs + "/nixos/modules/services/continuous-integration/buildbot/master.nix";
  buildbotWorkerModule =
    nixngInputs.nixpkgs + "/nixos/modules/services/continuous-integration/buildbot/worker.nix";
in
{
  options.nixos = lib.mkOption {
    type = lib.types.submodule {
      imports = [
        buildbotMasterModule
        buildbotWorkerModule
        nixngInputs.buildbot-nix.nixosModules.buildbot-master
        nixngInputs.buildbot-nix.nixosModules.buildbot-worker
      ];
    };
  };
}
