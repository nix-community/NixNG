# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ pkgs, lib, config, ... }:
{
  options.system.build = {
    toplevel = lib.mkOption {
      description = ''
          The full system, built up.
        '';
      type = lib.types.path;
    };
  };

  config.system.build.toplevel =
    pkgs.runCommandNoCC "nixng-${config.system.name}"
      { nativeBuildInputs = with pkgs; [ busybox makeWrapper ]; }
      (let
         closureInfo = pkgs.closureInfo
          { rootPaths = [ config.init.script config.system.activationScript ]; };
       in
         ''
           mkdir $out

           # Substitute in the path to the system closure to avoid
           # an infinite dep cycle
           substitute ${config.init.script} $out/init \
             --subst-var-by "systemConfig" "$out"
           substitute ${config.system.activationScript} $out/activation \
             --subst-var-by "systemConfig" "$out"
           chmod +x $out/init $out/activation
           echo "${pkgs.stdenv.system}" > $out/system

           #
           ${lib.optionalString config.system.createNixRegistration
             "ln -s ${closureInfo}/registration $out/registration"}
         '');
}
