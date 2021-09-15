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

{ pkgs, callPackage, system, dockerTools
, runCommandNoCC, lib, nglib
, busybox, config, name
}:

with lib;
let
  defaultModules = import ../modules ++ singleton
    ({ ... }: {
      system.name = name;
    });

  evaledModules = lib.evalModules
    { 
      modules = defaultModules ++ [ config ({ ... }:
        {
          _module.args = {
            inherit pkgs system nglib;
          };
        }
      )];
    };

  failedAssertions = map (x: x.message) (filter (x: !x.assertion) evaledModules.config.assertions);
  configValid =
    if failedAssertions != [] then
      throw "\nFailed assertions:\n${lib.concatStringsSep "\n" (map (x: "- ${x}") failedAssertions)}"
    else
      evaledModules.config;
in 
evaledModules // { config = configValid; }
