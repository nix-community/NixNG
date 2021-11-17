/*
  * NixNG
  * Copyright (c) 2021  GPL Magic_RB <magic_rb@redalder.org>
  *
  *  This file is free software: you may copy, redistribute and/or modify it
  *  under the terms of the GNU General Public License as published by the
  *  Free Software Foundation, either version 3 of the License, or (at your
  *  option) any later version.
  *
  *  This file is distributed in the hope that it will be useful, but
  *  WITHOUT ANY WARRANTY; without even the implied warranty of
  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  *  General Public License for more details.
  *
  *  You should have received a copy of the GNU General Public License
  *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

{ coreutils
, writeShellScriptBin
}:
writeShellScriptBin "install-wrapper"
  ''
    index=0
    params=( "$@" )

    for arg in "$@" ; do
      case "$arg" in
        "--mode")
          unset 'params[i]'
          unset 'params['"((i+1))"']'
          ;;
        "--owner")
          unset 'params[i]'
          unset 'params['"((i+1))"']'
          ;;
        "--group")
          unset 'params[i]'
          unset 'params['"((i+1))"']'
          ;;
        "-m")
          unset 'params[i]'
          unset 'params['"((i+1))"']'
          ;;
        "-o")
          unset 'params[i]'
          unset 'params['"((i+1))"']'
          ;;
        "-g")
          unset 'params[i]'
          unset 'params['"((i+1))"']'
          ;;
      esac

      if [[ "$arg" =~ ^\/([-_\.a-zA-Z0-9]*\/)([-_\.a-zA-Z0-9]*\/)*$ ]] ; then
         mkdir -p "$arg"
      fi

      ((i=i+1))
    done

    install -D "''${params[@]}"
  ''
