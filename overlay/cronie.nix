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

{ stdenv, lib, fetchurl }:
stdenv.mkDerivation {
  name = "cronie";
  version = "1.5.7";

  configureFlags = "--localstatedir=/var --sysconfdir=/etc";

  src = fetchurl {
    url = "https://github.com/cronie-crond/cronie/releases/download/cronie-1.5.7/cronie-1.5.7.tar.gz";
    sha256 = "sha256-U4vPry6Yblrh7fbRRyp36oJx1qkAWu4kl6ntbhMyDrM=";
  };
}
