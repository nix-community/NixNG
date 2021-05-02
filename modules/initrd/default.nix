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

{ pkgs, lib, config, nglib, ... }:
with lib;
let
  cfg = config.initrd;
in
{
  options.initrd = {
    enable = mkEnableOption "Enable initrd as init";
  };

  config.init = mkMerge [
    {
      availableInits = [ "initrd" ];
    }
    (mkIf cfg.enable {
      type = "initrd";
      script = nglib.writeSubstitutedShellScript {
        name = "init";
        file = ./init.sh;
        substitutes = with pkgs; {
          inherit eudev bash busybox;
        };
      };
    })
  ];
}
