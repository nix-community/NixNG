# SPDX-FileCopyrightText: 2021 Richard Brežák and NixNG contributors
# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  runCommandLocal,
  busybox,
  runtimeShell,
}:
{
  writeSubstitutedShellScript =
    {
      name,
      file,
      substitutes,
    }:
    runCommandLocal name ({ nativeBuildInputs = [ busybox ]; } // substitutes) ''
      TMPFILE=$(mktemp)
      substituteAll ${file} $TMPFILE
      touch $out
      echo "#! ${busybox}/bin/sh" > $out
      cat $TMPFILE >> $out
      chmod +x $out
    '';

  writeSubstitutedShellScriptBin =
    {
      name,
      file,
      substitutes,
    }:
    runCommandLocal name ({ nativeBuildInputs = [ busybox ]; } // substitutes) ''
      TMPFILE=$(mktemp)
      substituteAll ${file} $TMPFILE
      mkdir -p $out/bin && touch $out/bin/${name}
      echo "#! ${busybox}/bin/sh" > $out/bin/${name}
      cat $TMPFILE >> $out/bin/${name}
      chmod +x $out/bin/${name}
    '';

  writeSubstitutedFile =
    {
      name,
      file,
      substitutes,
    }:
    runCommandLocal name ({ nativeBuildInputs = [ busybox ]; } // substitutes) ''
      TMPFILE=$(mktemp)
      substituteAll ${file} $TMPFILE
      touch $out
      cat $TMPFILE >> $out
    '';
}
