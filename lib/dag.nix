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

{ fetchurl, lib, callPackage }:
callPackage
  (import (fetchurl {
    url = "https://raw.githubusercontent.com/nix-community/home-manager/45abf3d38a2b51c00c347cab6950f3734e023bba/modules/lib/dag.nix";
    sha256 = "sha256-NN9iKanf86D1MH9Nx8nsQj9T2+Poy9XeW9pLcZIyFHU=";
  }))
{ }
