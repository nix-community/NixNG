# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  pkgs,
  lib,
  config,
  ...
}:
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
    pkgs.runCommand "nixng-${config.networking.hostName}"
      {
        nativeBuildInputs = with pkgs; [
          busybox
          makeWrapper
        ];
      }
      (
        let
          closureInfo = pkgs.closureInfo {
            rootPaths = [
              config.init.script
              config.system.activationScript
            ];
          };
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
          ${lib.optionalString config.system.createNixRegistration "ln -s ${closureInfo}/registration $out/registration"}
        ''
      );
}
