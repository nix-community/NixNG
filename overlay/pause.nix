# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
