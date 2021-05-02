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

{ lib, ... }:
with lib;
{
  options.ids = {
    uids = mkOption {
      description = "A username to uid map, used for keeping track of assigned uids.";
      type = with types; attrsOf int;
      default = {
        root = 0;
        www-data = 54;
        postgres = 71;
        hydra = 122;
        hydra-queue-runner = 235;
        hydra-www = 236;
        gitea = 399; # might change!
        nobody = 65534;
      };
    };
    gids = mkOption {
      description = "A groupname to gid map, used for keeping track of assigned gids.";
      type = with types; attrsOf int;
      default = {
        root = 0;
        www-data = 54;
        postgres = 71;
        hydra = 122;
        hydra-queue-runner = 235;
        hydra-www = 236;
        log = 399;
        nogroup = 65534;
      };
    };
  };
}
