# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ lib, ... }:
let
  inherit (lib) mkOption types;
in
{
  options.assertions = mkOption {
    description = "List of assertions";
    type = types.listOf types.unspecified;
    default = [ ];
    example = [
      {
        assertion = 1 == 2;
        message = "The universe broke!";
      }
    ];
  };
}
