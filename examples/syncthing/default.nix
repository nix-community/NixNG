# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-syncthing";
  config = (
    { ... }:
    {
      dumb-init = {
        enable = true;
        type.services = { };
      };
      init.services.syncthing = {
        shutdownOnExit = true;
      };

      services.syncthing = {
        enable = true;
        guiAddress = "http://0.0.0.0:8384/";
      };
    }
  );
}
