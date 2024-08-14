# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
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
  name = "nixng-nix";

  config = ({ pkgs, ... }: {
    dumb-init = {
      enable = true;
      type.shell = { };
    };
    nix = {
      enable = true;
      package = pkgs.nixStable;
      config = {
        experimental-features = [ "nix-command" "flakes" ];
        sandbox = false;
      };
    };
  });
}
