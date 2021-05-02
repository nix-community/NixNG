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

{ pkgs, lib, config, options, nglib, system, ... }:
with lib;
let
  cfg = config.initramfs;
in
{
  options.initramfs = {
    enable = mkEnableOption "Enable initramfs generation";
    config = mkOption {
      description = "Configuration for the initramfs.";
      type = types.submoduleWith {
        modules = (import ../default.nix).initramfs;
        specialArgs = { inherit nglib pkgs; };
      };
      default = {
        initrd.enable = true;
      };
    };
    image = mkOption {
      description = "initramfs image.";
      type = types.unspecified;
      readOnly = true;
    };
  };

  config.initramfs.image = mkIf cfg.enable (nglib.makeSystem {
    inherit system;
    config = cfg.config;
    name = "initrd";
  }).initramfs;
}
