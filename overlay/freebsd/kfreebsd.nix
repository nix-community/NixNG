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
, lib

, fetchFromGitHub

, bmake
, python3
, which
, clang_10
}:
with lib;
let
  version = "13.0.0";
in
stdenv.mkDerivation {
  inherit version;
  name = "kfreebsd";

  postUnpack = ''
    find source -type f -exec sed -i 's/\(PATH=[[:space:]]\/sbin:\/bin:\/usr\/sbin:\/usr\/bin\)/# \1/' {} +

    sed -i 's~=[[:space:]]bootstrap_bmake(source_root, objdir_prefix)~= "'"$(which bmake)"'"~' source/tools/build/make.py
    cat source/tools/build/make.py | head -n 250 | tail -n 10
  '';

  architectures = [
    "arm"
    "x86"
    "i386"
    "mips"
    "amd64"
    "arm64"
    "riscv"
    "powerpc"
  ];

  buildPhase = ''
    export CPP="$(which cpp)" \
           CXX="$(which g++)" \
           CC="$(which gcc)" \
           XCPP="${clang_10}/bin/cpp" \
           XLD="${clang_10}/bin/ld" \
           MAKEOBJDIRPREFIX="$(mktemp -d)"

    python3 ./tools/build/make.py TARGET=amd64 \
                                  TARGET_ARCH=amd64 \
                                  --cross-bindir=${clang_10}/bin kernel-toolchain
  '';

  nativeBuildInputs =
    [
      bmake
      which
      python3
      clang_10
    ];

  src = fetchFromGitHub {
    owner = "freebsd";
    repo = "freebsd-src";
    rev = "release/" + version;
    sha256 = "sha256-2WYk/taxWc74uh2KJf9TzWDxUPrtkvt2nhU/qUZMu+Q=";
  };
}
