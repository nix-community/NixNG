# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-nix";

  config = (
    { pkgs, ... }:
    {
      dumb-init = {
        enable = true;
        type.shell = { };
      };
      nix = {
        enable = true;
        package = pkgs.nix;
        config = {
          experimental-features = [
            "nix-command"
            "flakes"
          ];
          sandbox = false;
        };
      };
    }
  );
}
