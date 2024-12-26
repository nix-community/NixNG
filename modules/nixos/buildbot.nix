{ lib, nixngInputs, ... }:
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
  imports = [
    (lib.mkAliasOptionModule
      [
        "services"
        "buildbot-master"
      ]
      [
        "nixos"
        "services"
        "buildbot-master"
      ]
    )
    (lib.mkAliasOptionModule
      [
        "services"
        "buildbot-worker"
      ]
      [
        "nixos"
        "services"
        "buildbot-worker"
      ]
    )
    (lib.mkAliasOptionModule
      [
        "services"
        "buildbot-nix"
        "master"
      ]
      [
        "nixos"
        "services"
        "buildbot-nix"
        "master"
      ]
    )
    (lib.mkAliasOptionModule
      [
        "services"
        "buildbot-nix"
        "worker"
      ]
      [
        "nixos"
        "services"
        "buildbot-nix"
        "worker"
      ]
    )
  ];
}
