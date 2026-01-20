# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ ... }:
{
  projectRootFile = "flake.nix";
  programs.nixfmt.enable = true;

  settings.formatter.nixfmt = {
    excludes = [ ];
    includes = [ "*.nix" ];
    options = [ ];
  };
}
