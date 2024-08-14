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
  name = "initrd";

  config = ({ pkgs, ... }: {
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
  });
}
