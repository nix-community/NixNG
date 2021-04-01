{ system
, runCommandNoCC, writeReferencesToFile
, path, name
}:

let
  references = writeReferencesToFile path;
in
runCommandNoCC name
  {}
  ''
    set -x
    shopt -s dotglob

    mkdir $out
    cp -r ${path}/* $out

    for path in $(cat ${references}); do
      if [[ -f $path ]]; then
        mkdir -p $out/$(dirname $path) ; cp -r $path $out/$path
      elif [[ -d $path ]]; then
        mkdir -p $out/$path ; cp -r $path/* $out/$path
      fi
    done
  ''
