self: super:
let
  callPackage = super.callPackage;
in
{
  bootloaderLinux = callPackage ./bootloader-linux.nix {};
}
