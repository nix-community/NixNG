# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, lib, config, nglib, ... }:
with lib;
let
  cfg = config.iana;
in
{
  options.iana = {
    enable = mkOption {
      description = "Enable /etc/services creation.";
      type = types.bool;
      default = true;
    };

    package = mkOption {
      description = "iana package.";
      type = types.package;
      default = pkgs.iana-etc;
    };
  };

  config = {
    system.activation."iana" = nglib.dag.dagEntryAnywhere ''
      export PATH=${pkgs.busybox}/bin

      mkdir -p /etc/
      ln -sf ${cfg.package}/etc/services /etc/services
      ln -sf ${cfg.package}/etc/protocols /etc/protocols
    '';
  };
}
