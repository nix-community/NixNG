self: super:
let
  callPackage = super.callPackage;
in
{
  tinyLinux = callPackage ./tiny-linux.nix {};
  runVmLinux = callPackage ./run-vm-linux.nix {};
  cronie = callPackage ./cronie.nix {};
}
