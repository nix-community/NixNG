{ lib, nglib, ... }:
let
  callPackage = lib.callPackageWith { inherit lib nglib; };
in
{
  dsl = callPackage ./dsl.nix { };
  generate = callPackage ./generator.nix { };
  ensureSomethings = callPackage ./ensure-somethings.nix { };
}
