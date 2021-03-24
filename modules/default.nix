{
  bootloader = [];
  system = [
    ./activation
    ./initramfs
    ./runit
    ./assertions.nix
    ./init.nix
    ./system.nix
  ];
  initramfs = [
    ./initrd
    ./assertions.nix
    ./init.nix
  ];
}
