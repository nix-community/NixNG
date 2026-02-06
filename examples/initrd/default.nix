# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{ nglib, nixpkgs, ... }:
nglib.makeSystem {
  inherit nixpkgs;
  system = "x86_64-linux";
  name = "initrd";

  config = (
    { pkgs, ... }:
    {
      runit = {
        enable = true;
      };

      fstab.entries = {
        "/" = {
          type = "ext4";
          device = "/dev/sda1";
        };
      };
      users.users.root.hashedPassword = "$5$Ws9piKbYzt9S6p1R$X4L6xn5UNQnufJUc/K5sKRE.0GuMR.8vp2BwIiglVYB"; # toor

      services.getty."ttyS0" = {
        port = "ttyS0";
        baudrate = "115200,38400,9600";
        options = {
          login-program = "${pkgs.busybox}/bin/login";
          login-options = [ "\\u" ];
        };
      };
    }
  );
}
