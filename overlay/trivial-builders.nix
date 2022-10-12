# SPDX-FileCopyrightText:  2021 Richard Brežák and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0
#
#   This Source Code Form is subject to the terms of the Mozilla Public
#   License, v. 2.0. If a copy of the MPL was not distributed with this
#   file, You can obtain one at http://mozilla.org/MPL/2.0/.

{ runCommandLocal
, busybox
, runtimeShell
}:
{
  writeSubstitutedShellScript =
    { name
    , file
    , substitutes
    }:
    runCommandLocal name
      ({
        nativeBuildInputs = [ busybox ];
      } // substitutes)
      ''
        TMPFILE=$(mktemp)
        substituteAll ${file} $TMPFILE
        touch $out
        echo "#! ${runtimeShell}" > $out
        cat $TMPFILE >> $out
        chmod +x $out
      '';

  writeSubstitutedShellScriptBin =
    { name
    , file
    , substitutes
    }:
    runCommandLocal name
      ({
        nativeBuildInputs = [ busybox ];
      } // substitutes)
      ''
        TMPFILE=$(mktemp)
        substituteAll ${file} $TMPFILE
        mkdir -p $out/bin && touch $out/bin/${name}
        echo "#! ${runtimeShell}" > $out/bin/${name}
        cat $TMPFILE >> $out/bin/${name}
        chmod +x $out/bin/${name}
      '';


  writeSubstitutedFile =
    { name
    , file
    , substitutes
    }:
    runCommandLocal name
      ({
        nativeBuildInputs = [ busybox ];
      } // substitutes)
      ''
        TMPFILE=$(mktemp)
        substituteAll ${file} $TMPFILE
        touch $out
        cat $TMPFILE >> $out
      '';
}
