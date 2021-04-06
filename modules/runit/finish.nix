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

  ${optionalString (s.finish != null && !s.shutdownOnExit) "exec ${s.finish}"}
  ${optionalString (s.finish != null && s.shutdownOnExit) "${s.finish}"}
  
  ${optionalString (s.shutdownOnExit) ("exec ${cfgInit.shutdown}")} 
''
