# SPDX-FileCopyrightText:  2025 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

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
