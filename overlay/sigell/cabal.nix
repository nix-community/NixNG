# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  mkDerivation,
  aeson,
  base,
  bytestring,
  lib,
  unix,
  unordered-containers,
  vector,
  cmdargs,
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
