# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{
  stdenv,
  # Path to the nixpkgs checkout
  path,
  meson,
  ninja,
  fetchFromGitHub,

  python3,
  gperf,
  libcap,
  pkg-config,
  util-linux,
  getent,
  libxcrypt,

  # Required for man
  libxslt,
  docbook_xsl,
  docbook_xml_dtd_42,
  docbook_xml_dtd_45,
}:
let
  version = "251.7";
in
stdenv.mkDerivation {
  pname = "systemd-tmpfiles";
  inherit version;

  nativeBuildInputs = [
    meson
    ninja

    (python3.withPackages (p: [ p.jinja2 ]))
    gperf
    libcap
    pkg-config
    util-linux
    getent
    libxcrypt

    libxslt
    docbook_xsl
    docbook_xml_dtd_42
    docbook_xml_dtd_45
  ];

  src = fetchFromGitHub {
    owner = "systemd";
    repo = "systemd-stable";
    rev = "v${version}";
    sha256 = "sha256-Sa5diyNFyYtREo1xSCcufAW83ZZGZvueoDVuQ2r8wno=";
  };

  mesonFlags = [
    "-Dversion-tag=${version}"
    # Fixup install paths
    "-Ddbuspolicydir=${placeholder "out"}/share/dbus-1/system.d"
    "-Ddbussessionservicedir=${placeholder "out"}/share/dbus-1/services"
    "-Ddbussystemservicedir=${placeholder "out"}/share/dbus-1/system-services"
    "-Dpamconfdir=${placeholder "out"}/etc/pam.d"
    "-Drootprefix=${placeholder "out"}"
    "-Dpkgconfiglibdir=${placeholder "dev"}/lib/pkgconfig"
    "-Dpkgconfigdatadir=${placeholder "dev"}/share/pkgconfig"
    "-Dinstall-sysconfdir=false"
    # Make systemd not look for sysvinit scripts and such
    "-Dsysvinit-path="
    "-Dsysvrcnd-path="
    # Don't create log dirs during the install (why is this even a thing??)
    "-Dcreate-log-dirs=false"
    # Upstream defaulted to disable manpages since they optimize for the much
    # more frequent development builds
    "-Dman=true"
    # While we disable all the tests, we also don't want to build them
    "-Dtests=false"
    # Link staticly with libsystemd-shared
    "-Dlink-boot-shared=false"
    "-Dlink-udev-shared=false"
    "-Dstandalone-binaries=true"
    # Disable all optional features
    "-Dadm-group=false"
    "-Danalyze=false"
    "-Dapparmor=false"
    "-Daudit=false"
    "-Dbacklight=false"
    "-Dbinfmt=false"
    "-Dbpf-framework=false"
    "-Dbzip2=false"
    "-Dcoredump=false"
    "-Ddbus=false"
    "-Delfutils=false"
    "-Denvironment-d=false"
    "-Dfdisk=false"
    "-Dgcrypt=false"
    "-Dglib=false"
    "-Dgshadow=false"
    "-Dgnutls=false"
    "-Dhibernate=false"
    "-Dhostnamed=false"
    "-Didn=false"
    "-Dima=false"
    "-Dinitrd=false"
    "-Dfirstboot=false"
    "-Dldconfig=false"
    "-Dlibcryptsetup=false"
    "-Dlibcurl=false"
    "-Dlibfido2=false"
    "-Dlibidn=false"
    "-Dlibidn2=false"
    "-Dlibiptc=false"
    "-Dlocaled=false"
    "-Dlogind=false"
    "-Dlz4=false"
    "-Dmachined=false"
    "-Dmicrohttpd=false"
    "-Dnetworkd=false"
    "-Dnscd=false"
    "-Dnss-myhostname=false"
    "-Dnss-resolve=false"
    "-Dnss-systemd=false"
    "-Doomd=false"
    "-Dopenssl=false"
    "-Dp11kit=false"
    "-Dpam=false"
    "-Dpcre2=false"
    "-Dpolkit=false"
    "-Dportabled=false"
    "-Dpstore=false"
    "-Dpwquality=false"
    "-Drandomseed=false"
    "-Dresolve=false"
    "-Drfkill=false"
    "-Dseccomp=false"
    "-Dsmack=false"
    "-Dsysext=false"
    "-Dtimedated=false"
    "-Dtimesyncd=false"
    "-Dtpm=false"
    "-Dqrencode=false"
    "-Dquotacheck=false"
    "-Duserdb=false"
    "-Dutmp=false"
    "-Dvconsole=false"
    "-Dwheel-group=false"
    "-Dxdg-autostart=false"
    "-Dxkbcommon=false"
    "-Dxz=false"
    "-Dzlib=false"
    "-Dzstd=false"
    "-Dkmod=false"
    "-Dacl=false"
  ];

  ninjaFlags = [
    "systemd-tmpfiles.standalone"
    "man/tmpfiles.d.5"
    "man/systemd-tmpfiles.8"
  ];

  postPatch =
    # Finally, patch shebangs in scripts used at build time. This must not patch
    # scripts that will end up in the output, to avoid build platform references
    # when cross-compiling.
    ''
      shopt -s extglob
      patchShebangs tools test src/!(rpm)
    '';

  outputs = [
    "out"
    "man"
    "dev"
  ];

  doCheck = false;

  # trigger the test -n "$DESTDIR" || mutate in upstreams build system
  preInstall = ''
    export DESTDIR=/
  '';
}
