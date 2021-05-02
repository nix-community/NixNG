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
{ n, s }:
with lib;
writeShellScript "${n}-run" ''
  ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
    with cv;
      ''
        if ! [[ -e ${dst} ]] ; then
          echo '${n}: linking `${src}` to `${dst}`'
          mkdir -p "$(dirname '${dst}')"
          ln -s '${src}' '${dst}'
        fi
      ''
  ) s.ensureSomething.link)}  

  ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
    with cv;
    ''
      if ! [[ -e ${dst} ]] ; then
        echo '${n}: copying `${src}` to `${dst}`'
        mkdir -p "$(dirname '${dst}')"
        cp -r '${src}' '${dst}'
      fi
    ''
  ) s.ensureSomething.copy)}  

  ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
    abort "linkFarm is not implemented yet in runit!"
  ) s.ensureSomething.linkFarm)}  

  ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
    with cv;
    ''
      if ! [[ -e ${dst} ]] ; then
        echo '${n}: executing `${executable}` to create `${dst}`'
        mkdir -p "$(dirname '${dst}')"
        out=${dst} ${executable}
        
        if ! [[ -e ${dst} ]] ; then
          echo '${n}: executed `${executable}` but `${dst}` does not exist!'
          exit 1
        fi
      fi
    ''
  ) s.ensureSomething.exec)}  

  ${concatStringsSep "\n" (mapAttrsToList (cn: cv:
    with cv;
    ''
      if ! [[ -e ${dst} ]] ; then
        echo '${n}: creating `${dst}`'

        ${if (type == "directory") then
          "mkdir -p ${dst}"
          else if (type == "file") then
            ''
              mkdir -p "$(dirname '${dst}')"
              touch ${dst}
            ''
          else
            abort "Unsupported init create type, module system should have caught this!"
         } 
        
        chown ${owner} ${dst}
        ${optionalString (mode != null) "chmod ${mode} ${dst}"}
      fi
    ''
  ) s.ensureSomething.create)}

  (
    cd ${s.pwd}
    export ${concatStringsSep " " (mapAttrsToList (n: v: "${n}=${v}") s.environment)}
    exec ${s.script}
  )
''
