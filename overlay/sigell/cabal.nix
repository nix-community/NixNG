# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ mkDerivation
, aeson
, base
, bytestring
, lib
, unix
, unordered-containers
, vector
, cmdargs
}:
mkDerivation {
  pname = "sigell";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson
    base
    bytestring
    unix
    unordered-containers
    vector
    cmdargs
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
