#
# NixNG
# Copyright (c) 2021  GPL Magic_RB <magic_rb@redalder.org>
#
#  This file is free software: you may copy, redistribute and/or modify it
#  under the terms of the GNU General Public License as published by the
#  Free Software Foundation, either version 3 of the License, or (at your
#  option) any later version.
#
#  This file is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#  General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

function addBmakeFlags {
    export INSTALL="install-wrapper"
    export LEX="flex"

    export INSTALL_LINK="ln -s"
    export INSTALL_SYMLINK="ln"
    export MINSTALL='${INSTALL} ${TAG_ARGS}'
    export NOT_FOR_TEST_SUITE="1"

    export STRIP="--strip"

    bmakeFlagsArray+=( "LOCALEDIR=${!outputLib}/share/locale" \
		       "BINDIR=${!outputBin}/bin" \
		       "LIBDIR=${!outputLib}/lib" \
		       "SHLIBDIR=${!outputLib}/lib" \
		       "MANDIR=${!outputMan}/share/man" \
		       "INFODIR=${!outputInfo}/share/infos" \
		       "DOCDIR=${!outputDoc}/share/doc" \
		       "TESTSDIR=$out/share/test" \
		       "BINOWN=root" "BINGRP=root" "BINMODE=755" \
				     "DTBGRP=root" \
				     "EFIGRP=root" \
				     "SHAREGRP=root" \
				     "CONFGRP=root" \
				     "DIRGRP=root" )
}

function bmakeBuildPhase {
    runHook preBuild

    local _include_dir="$(mktemp -d)"

    for arch in ''${architectures[@]} ; do
	ln -s $PWD/sys/$arch/include $_include_dir/$arch
    done
    ln -s $PWD/sys/amd64/include $_include_dir/machine

    local _cflags="-L @libbsd@/lib -lbsd -I@libbsd@/include/bsd -DLIBBSD_OVERLAY=1"
    local _cflags="$_cflags"

    CFLAGS="$CFLAGS $_cflags" bmake -C ${bmakeDir:-.} $bmakeFlags "${bmakeFlagsArray[@]}"

    runHook postBuild
}

function bmakeInstallPhase {
    runHook preInstall

    bmake -C ${bmakeInstallDir:-${bmakeDir:-.}} install ${bmakeInstallFlags:-${bmakeFlags}} "${bmakeInstallFlagsArray[@]:-${bmakeFlagsArray[@]}}"

    runHook postInstall
}

if [ -z "${dontUseBmakeBuild-}" -a -z "${buildPhase-}" ]; then
    buildPhase=bmakeBuildPhase
fi

if [ -z "${dontUseBmakeInstall-}" -a -z "${installPhase-}" ]; then
    installPhase=bmakeInstallPhase
fi

preConfigureHooks+=(addBmakeFlags)
