# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ stdenv, fetchurl }:
stdenv.mkDerivation {
  name = "cronie";
  version = "1.5.7";

  configureFlags = [
    "--localstatedir=/var"
    "--sysconfdir=/etc"
  ];

  src = fetchurl {
    url = "https://github.com/cronie-crond/cronie/releases/download/cronie-1.5.7/cronie-1.5.7.tar.gz";
    sha256 = "sha256-U4vPry6Yblrh7fbRRyp36oJx1qkAWu4kl6ntbhMyDrM=";
  };
}
