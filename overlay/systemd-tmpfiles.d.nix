# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ stdenv
, systemdStandalone
}:
stdenv.mkDerivation {
  inherit (systemdStandalone) pname version;

  dontFetch = true;
  dontUnpack = true;
  dontPatch = true;
  dontConfigure = true;
  dontBuild = true;
  dontInstall = false;
  dontFixup = true;

  outputs = [ "out" "man" ];

  installPhase = ''
    install ${systemdStandalone}/bin/systemd-tmpfiles.standalone -Dt $out/bin
    ln -s ${systemdStandalone}/bin/systemd-tmpfiles.standalone $out/bin/tmpfiles.d

    install ${systemdStandalone.man}/share/man/man8/systemd-tmpfiles.8.gz -Dt $man/share/man/man8
    install ${systemdStandalone.man}/share/man/man5/tmpfiles.d.5.gz -Dt $man/share/man/man5
  '';
}
