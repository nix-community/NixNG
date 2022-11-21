# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ stdenv, lib, fetchurl }:
stdenv.mkDerivation {
  name = "cronie";
  version = "1.5.7";

  configureFlags = ["--localstatedir=/var" "--sysconfdir=/etc"];

  src = fetchurl {
    url = "https://github.com/cronie-crond/cronie/releases/download/cronie-1.5.7/cronie-1.5.7.tar.gz";
    sha256 = "sha256-U4vPry6Yblrh7fbRRyp36oJx1qkAWu4kl6ntbhMyDrM=";
  };
}
