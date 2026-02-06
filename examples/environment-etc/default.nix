# SPDX-FileCopyrightText: 2025 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-environment-etc";

  config = (
    { pkgs, ... }:
    {
      dinit.enable = true;
      environment.etc."textual-content".text = "test content";
      environment.etc."source-link".source = pkgs.writeText "test-target" ''
        Some meaningless content.
      '';
      environment.etc."source-copy" = {
        mode = "0700";
        user = "nobody";
        group = "nogroup";
        text = ''
          some other test content
        '';
      };
    }
  );
}
