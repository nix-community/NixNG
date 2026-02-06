# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

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
