# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ stdenv, kubernetes }:
stdenv.mkDerivation {
  pname = "pause";
  inherit (kubernetes) version src;

  dontConfigure = true;

  patchPhase = ''
    sed -i 's/getpid() != 1/0/' build/pause/linux/pause.c
  '';

  buildPhase = ''
    cc build/pause/linux/pause.c -o pause
  '';

  installPhase = ''
    install -D pause -t $out/bin
  '';
}
