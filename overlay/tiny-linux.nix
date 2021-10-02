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

# TODO use linuxManualConfig instead of buildLinux

{ buildLinux
, linux
, pkg-config
, ncurses
, writeText
, runCommand
, lib
, extraConfig ? { }
}:
let
  configfile = writeText "config" ''
    ${lib.concatStringsSep "\n" (lib.mapAttrsToList (n: v: "${n}=${v}") extraConfig)}
  '';
  origKernel = buildLinux {
    inherit (linux) src version stdenv;
    inherit configfile;

    kernelPatches = [ ];
    allowImportFromDerivation = true;
  };
  self =
    origKernel.overrideAttrs (old: {
      buildFlags = [
        "KBUILD_BUILD_VERSION=1-NixNG"
        "bzImage"
        "vmlinux"
      ];
      configurePhase = ''
        runHook preConfigure

        mkdir build
        export buildRoot="$(pwd)/build"

        echo "manual-config configurePhase buildRoot=$buildRoot pwd=$PWD"

        runHook postConfigure

        # make $makeFlags "''${makeFlagsArray[@]}" mrproper

        make $makeFlags "''${makeFlagsArray[@]}" tinyconfig
        if [[ -f $buildRoot/.config ]] ; then
          cat ${configfile} >> $buildRoot/.config
          cat $buildRoot/.config
        else
          echo "$buildRoot/.config is empty, not appending"
          exit 1
        fi
        echo $buildFlags

        # make $makeFlags "''${makeFlagsArray[@]}" prepare

        # Note: https://github.com/NixOS/nixpkgs/blob/9c213398b312e0f0bb9cdf05090fd20223a82ad0/pkgs/os-specific/linux/kernel/manual-config.nix#L166
        buildFlagsArray+=("KBUILD_BUILD_TIMESTAMP=$(date -u -d @$SOURCE_DATE_EPOCH)")      
      
        ls build
      '';

      buildPhase = "";

      postInstall = "";

      nativeBuildInputs = old.nativeBuildInputs ++ [ pkg-config ncurses ];
    });
in
self
