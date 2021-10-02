/*
  * NixNG
  * Copyright (c) 2021  GPL Magic_RB <magic_rb@redalder.org>
  *
  *  This file is free software: you may copy, redistribute and/or modify it
  *  under the terms of the GNU General Public License as published by the
  *  Free Software Foundation, either version 3 of the License, or (at your
  *  option) any later version.
  *
  *  This file is distributed in the hope that it will be useful, but
  *  WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  *  General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

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
