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

  exec ${s.script}
''
