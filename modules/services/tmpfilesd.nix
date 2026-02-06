# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  config,
  pkgs,
  lib,
  nglib,
  ...
}:
let
  cfg = config.services.tmpfilesd;
in
{
  options.services.tmpfilesd = {
    enable = lib.mkEnableOption "Enable tmpfiles.d";
    useCron = lib.mkEnableOption "Use cron for tmpfiles.d operations";

    entries = lib.mkOption {
      default = { };
      type = with lib.types; attrsOf (listOf (listOf str));
      description = ''
        A attribute set of lists of lists. The outer attribute set corresponds to files,
        the first list corresponds to the individual entries and the most inner one
         corresponds to the individual fields in use by tmpfiles.d.
      '';
      example = ''
        {
          example = [
            # Type  Path          Mode   User   Group   Age   Argument...
            [ "d"     "/run/user"   "0755" "root" "root"  "10d" "-" ]
            [ "L"     "/tmp/foobar" "-"    "-"    "-"     "-"   "/dev/null" ]
          ]
        }
      '';
      apply = lib.mkApply (
        x:
        pkgs.runCommand "" { } ''
          mkdir -p $out
          ${lib.concatMapStringsSep "\n\n" (
            { name, value }:
            "cat > $out/${name}.conf <<EOF\n"
            + (lib.concatMapStringsSep "\n" (lib.concatStringsSep " ") value)
            + "\nEOF\n"
          ) (lib.mapAttrsToList lib.nameValuePair x)}
        ''
      );
    };

    package = lib.mkOption {
      type = lib.types.path;
      description = ''
        The package used by this service, opentmpfilesd is unmaintained so the
        only option is the one shipped with systemd. Thankfully it is possible
        to get a standalone version which does not depend on systemd.s
      '';
      default = pkgs.systemdTmpfilesD;
    };
  };

  config = lib.mkIf cfg.enable {
    init.services.tmpfilesd = {
      script = pkgs.writeShellScript "tmpfilesd-run" ''
        ${cfg.package}/bin/tmpfiles.d --create $(find ${cfg.entries.applied} -name '*.conf' -maxdepth 1)
        exec ${pkgs.pause}/bin/pause
      '';
      enabled = true;
    };

    assertions = [
      {
        assertion = (cfg.entries == { }) || (cfg.entries != { } && cfg.enable == true);
        message = ''
          If `services.tmpfilesd.entries` is not `{}` then `services.tmpfilesd.enable` must
          be `true` as other modules may misbehave.
        '';
      }
    ];
  };
}
