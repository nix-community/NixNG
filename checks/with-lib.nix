# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  ripgrep,
  runCommand,
  self,
  lib,
}:
{
  allowed ? [ ],
}:
let
  allowed' = lib.pipe allowed [
    (map (lib.replaceStrings [ "." ] [ "\." ]))
    (lib.concatStringsSep "|")
  ];
in
runCommand "with-lib-check.sh" { nativeBuildInputs = [ ripgrep ]; } ''
  [ "$(rg "with lib;" -n ${self} | rg -v "${allowed'}" | tee $out | wc -l)" -le 1 ] \
    || ( cat $out ; exit 1 ) \
    && exit 0
''
