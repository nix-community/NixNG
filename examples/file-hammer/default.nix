# SPDX-FileCopyrightText: 2025 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "nixng-file-hammer";

  config = (
    { pkgs, ... }:
    {
      dinit.enable = true;

      services.file-hammer."/test" = {
        specification = {
          ignores = [ ];

          directory = {
            owner = {
              user."UserName" = "nobody";
              group."GroupName" = "nogroup";
            };

            mode = 0;

            content."DirectoryContentManaged" = {
              files = {
                "testfoo" = {
                  owner = {
                    user."UserName" = "root";
                    group."GroupName" = "root";
                  };

                  mode = 0;

                  content."ContentText" = "hello!";
                };
              };
              directories = { };
              links = { };
            };
          };
        };
      };
    }
  );
}
