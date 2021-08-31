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

{ lib
, writeShellScript
}:
{ n, s, cfgInit }:
with lib;
writeShellScript "${n}-finish" ''
  ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
    with cv;
    optionalString (!cv.persistent) ''
      if [[ -e ${dst} ]] ; then
        echo '${n}: removing non-presistent `${dst}`'
        rm -v ${dst}
      fi
    ''
  ) s.ensureSomething.link)}

  ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
    with cv;
    optionalString (!cv.persistent) ''
      if [[ -e ${dst} ]] ; then
        echo '${n}: removing non-presistent `${dst}`'
        rm -rv ${dst}
      fi
    ''
  ) s.ensureSomething.copy)}

  ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
    abort "linkFarm is not implemented yet in runit!"
  ) s.ensureSomething.linkFarm)}  

  ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
    with cv;
    optionalString (!cv.persistent) ''
      if [[ -e ${dst} ]] ; then
        echo '${n}: removing non-persistent `${dst}`'
        rm -rv '${dst}'
      fi
    ''
  ) s.ensureSomething.exec)}  

  ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
    with cv;
    optionalString (!cv.persistent) ''
      if [[ -e ${dst} ]] ; then
        echo '${n}: removing non-persistent `${dst}`'

        ${if (type == "directory") then
          "rm -rv ${dst}"
          else if (type == "file") then
            ''
              rm -v ${dst}
            ''
          else
            abort "Unsupported init create type, module system should have caught this!"
         } 
      fi
    ''
  ) s.ensureSomething.create)}

  (
    cd ${s.pwd}
    ${optionalString (s.environment != {}) "export ${concatStringsSep " " (mapAttrsToList (n: v: "${n}=${v}") s.environment)}"}
    ${optionalString (s.finish != null && !s.shutdownOnExit) "exec ${s.finish}"}
    ${optionalString (s.finish != null && s.shutdownOnExit) "${s.finish}"}
  )
  
  ${optionalString (s.shutdownOnExit) ("exec ${cfgInit.shutdown}")} 
''
