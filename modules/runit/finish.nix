# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ lib, writeShellScript }:
{
  n,
  s,
  cfgInit,
}:
writeShellScript "${n}-finish" ''
  ${lib.concatStringsSep "\n" (
    lib.mapAttrsToList (
      cn: cv:
      with cv;
      lib.optionalString (!cv.persistent) ''
        if [[ -e ${dst} ]] ; then
          echo '${n}: removing non-presistent `${dst}`'
          rm -v ${dst}
        fi
      ''
    ) s.ensureSomething.link
  )}

  ${lib.concatStringsSep "\n" (
    lib.mapAttrsToList (
      cn: cv:
      with cv;
      lib.optionalString (!cv.persistent) ''
        if [[ -e ${dst} ]] ; then
          echo '${n}: removing non-presistent `${dst}`'
          rm -rv ${dst}
        fi
      ''
    ) s.ensureSomething.copy
  )}

  ${lib.concatStringsSep "\n" (
    lib.mapAttrsToList (
      cn: cv: abort "linkFarm is not implemented yet in runit!"
    ) s.ensureSomething.linkFarm
  )}

  ${lib.concatStringsSep "\n" (
    lib.mapAttrsToList (
      cn: cv:
      with cv;
      lib.optionalString (!cv.persistent) ''
        if [[ -e ${dst} ]] ; then
          echo '${n}: removing non-persistent `${dst}`'
          rm -rv '${dst}'
        fi
      ''
    ) s.ensureSomething.exec
  )}

  ${lib.concatStringsSep "\n" (
    lib.mapAttrsToList (
      cn: cv:
      with cv;
      lib.optionalString (!cv.persistent) ''
        if [[ -e ${dst} ]] ; then
          echo '${n}: removing non-persistent `${dst}`'

          ${
            if (type == "directory") then
              "rm -rv ${dst}"
            else if (type == "file") then
              ''
                rm -v ${dst}
              ''
            else
              abort "Unsupported init create type, module system should have caught this!"
          }
        fi
      ''
    ) s.ensureSomething.create
  )}

  (
    cd ${s.workingDirectory}
    ${
      lib.optionalString (s.environment != { })
        "export ${lib.concatStringsSep " " (lib.mapAttrsToList (n: v: "${n}=${v}") s.environment)}"
    }
    ${lib.optionalString (s.execStop != null && !s.shutdownOnExit) "exec ${s.execStop}"}
    ${lib.optionalString (s.execStop != null && s.shutdownOnExit) "${s.execStop}"}
  )

  ${lib.optionalString (s.shutdownOnExit) ("exec ${cfgInit.shutdown}")}
''
