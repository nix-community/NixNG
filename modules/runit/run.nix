# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{
  lib,
  nglib,
  writeShellScript,
}:
{ n, s }:
writeShellScript "${n}-run" ''
  ${lib.concatStringsSep "\n" (
    map (dependency: ''
      if ! sv check "${dependency}" ; then
        exit -1
      fi
    '') s.dependencies
  )}

  ${lib.concatStringsSep "\n" (
    lib.mapAttrsToList (
      cn: cv: with cv; ''
        if ! [[ -e ${dst} ]] ; then
          echo '${n}: linking `${src}` to `${dst}`'
          mkdir -p "$(dirname '${dst}')"
          ln -s '${src}' '${dst}'
        fi
      ''
    ) s.ensureSomething.link
  )}

  ${lib.concatStringsSep "\n" (
    lib.mapAttrsToList (
      cn: cv: with cv; ''
        if ! [[ -e ${dst} ]] ; then
          echo '${n}: copying `${src}` to `${dst}`'
          mkdir -p "$(dirname '${dst}')"
          cp -r '${src}' '${dst}'
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
      cn: cv: with cv; ''
        if ! [[ -e ${dst} ]] ; then
          echo '${n}: executing `${executable}` to create `${dst}`'
          mkdir -p "$(dirname '${dst}')"
          out=${dst} ${executable}

          if ! [[ -e ${dst} ]] ; then
            echo '${n}: executed `${executable}` but `${dst}` does not exist!'
            exit 1
          fi
        fi
      ''
    ) s.ensureSomething.exec
  )}

  ${lib.concatStringsSep "\n" (
    lib.mapAttrsToList (
      cn: cv: with cv; ''
        if ! [[ -e ${dst} ]] ; then
          echo '${n}: creating `${dst}`'

          ${
            if (type == "directory") then
              "mkdir -p ${dst}"
            else if (type == "file") then
              ''
                mkdir -p "$(dirname '${dst}')"
                touch ${dst}
              ''
            else
              abort "Unsupported init create type, module system should have caught this!"
          }

          chown ${owner} ${dst}
          ${lib.optionalString (mode != null) "chmod ${mode} ${dst}"}
        fi
      ''
    ) s.ensureSomething.create
  )}

  cd ${s.workingDirectory}
  ${lib.optionalString (s.environment != { })
    "export ${lib.concatStringsSep " " (lib.mapAttrsToList (n: v: "${n}=${v}") s.environment)}"
  }
  exec ${nglib.maybeChangeUserAndGroup s.user s.group s.execStart}
''
