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

{ stdenv
, freebsd-compat
, freebsd-file2c
, yacc
, flex
, fetchFromGitHub
}:
let
  version = "13.0.0";
in
stdenv.mkDerivation {
  inherit version;
  pname = "config";

  CFLAGS = "-Wno-error=cast-qual";

  bmakeDir = "usr.sbin";
  bmakeFlags = "-m ../share/mk config";

  bmakeInstallDir = "usr.sbin/config";
  bmakeInstallFlags = "-m ../../share/mk";

  nativeBuildInputs =
    [ yacc flex freebsd-file2c ];
  buildInputs = [ freebsd-compat ];

  src = fetchFromGitHub {
    owner = "freebsd";
    repo = "freebsd-src";
    rev = "release/" + version;
    sha256 = "sha256-2WYk/taxWc74uh2KJf9TzWDxUPrtkvt2nhU/qUZMu+Q=";
  };
}
