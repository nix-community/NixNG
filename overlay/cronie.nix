# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ stdenv, autoconf, automake, fetchurl, fetchFromGitHub }:
stdenv.mkDerivation {
  name = "cronie";
  version = "unstable-2026-06-03";

  buildInputs = [
    autoconf
    automake
  ];

  preConfigure = ''
    patchShebangs autogen.sh
    ./autogen.sh
  '';

  configureFlags = [
    "--localstatedir=/var"
    "--sysconfdir=/etc"
  ];

  src = fetchFromGitHub {
    owner = "cronie-crond";
    repo = "cronie";
    rev = "5f9f16b5663becefdd0dd70df31c0ef5ac36f943";
    hash = "sha256-0ZuohRl97kqLi5FrgaAeOaCaDLlYUV3pY2MI7KAr1S0=";
  };
}
