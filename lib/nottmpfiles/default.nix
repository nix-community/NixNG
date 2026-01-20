# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ lib, nglib, ... }:
let
  callPackage = lib.callPackageWith { inherit lib nglib; };
in
{
  dsl = callPackage ./dsl.nix { };
  generate = callPackage ./generator.nix { };
  ensureSomethings = callPackage ./ensure-somethings.nix { };
}
