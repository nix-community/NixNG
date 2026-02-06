# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ stdenv, systemd }:
stdenv.mkDerivation {
  inherit (systemd) pname version;

  dontFetch = true;
  dontUnpack = true;
  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;
  dontInstall = false;
  dontFixup = true;

  outputs = [
    "out"
    "man"
  ];

  installPhase = ''
    install ${systemd}/bin/systemd-tmpfiles -Dt $out/bin
    ln -s ${systemd}/bin/systemd-tmpfiles $out/bin/tmpfiles.d

    install ${systemd.man}/share/man/man8/systemd-tmpfiles.8.gz -Dt $man/share/man/man8
    install ${systemd.man}/share/man/man5/tmpfiles.d.5.gz -Dt $man/share/man/man5
  '';
}
