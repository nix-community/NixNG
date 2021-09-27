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
    };
    gids = mkOption {
      description = "A groupname to gid map, used for keeping track of assigned gids.";
      type = with types; attrsOf int;
    };
  };

  config = {
    ids.uids = {
        root = 0;
        postfix = 13;
        #postdrop = 14;
        dovecot = 46;
        dovenull = 47;
        www-data = 54;
        nginx = 60;
        postgres = 71;
        hydra = 122;
        hydra-queue-runner = 235;
        hydra-www = 236;
        pantalaimon = 398; # might change!
        gitea = 399; # might change!
        jmusicbot = 400;
        vmail = 5000;
        nobody = 65534;
      };

    ids.gids = {
        root = 0;
        postfix = 13;
        postdrop = 14;
        dovecot = 46;
        dovenull = 47;
        www-data = 54;
        nginx = 60;
        postgres = 71;
        hydra = 122;
        hydra-queue-runner = 235;
        hydra-www = 236;
        pantalaimon = 398; # might change!
        log = 399;
        jmusicbot = 400;
        vmail = 5000;
        nogroup = 65534;
      };
  };
}
