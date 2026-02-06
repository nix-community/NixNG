# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-jmusicbot";
  config = (
    { ... }:
    {
      dumb-init = {
        enable = true;
        type.services = { };
      };
      init.services.jmusicbot = {
        shutdownOnExit = true;
      };

      services.jmusicbot = {
        enable = true;

        config = {
          prefix = "sudo";
          token = "\${BOT_TOKEN}";
          owner = "\${BOT_OWNER}";
        };
      };
    }
  );
}
