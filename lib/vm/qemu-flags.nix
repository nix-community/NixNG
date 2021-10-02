{ pkgs }:
{
  # from: https://github.com/NixOS/nixpkgs/blob/master/nixos/lib/qemu-flags.nix
  qemuBinary = qemu: {
    x86_64-linux = "${qemu}/bin/qemu-kvm -cpu max";
    armv7l-linux = "${qemu}/bin/qemu-system-arm -enable-kvm -machine virt -cpu host";
    aarch64-linux = "${qemu}/bin/qemu-system-aarch64 -enable-kvm -machine virt,gic-version=host -cpu host";
    powerpc64le-linux = "${qemu}/bin/qemu-system-ppc64 -machine powernv";
    powerpc64-linux = "${qemu}/bin/qemu-system-ppc64 -machine powernv";
    x86_64-darwin = "${qemu}/bin/qemu-kvm -cpu max";
  }."${pkgs.stdenv.hostPlatform.system}" or "${qemu}/bin/qemu-kvm";

  qemuSerialDevice =
    if pkgs.stdenv.isi686 || pkgs.stdenv.isx86_64 then "ttyS0"
    else if (with pkgs.stdenv.hostPlatform; isAarch32 || isAarch64 || isPower) then "ttyAMA0"
    else throw "Unknown QEMU serial device for system '${pkgs.stdenv.hostPlatform.system}'";
}
