self: super:
let
  callPackage = super.callPackage;
in
{
  bootloaderLinux = callPackage ./bootloader-linux.nix {};
  tinyLinux = callPackage ./tiny-linux.nix {};
  runVmLinux = callPackage ./run-vm-linux.nix {};
}
