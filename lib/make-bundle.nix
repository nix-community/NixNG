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
    shopt -s dotglob

    mkdir $out
    cp -r ${path}/* $out

    for path in $(cat ${references}); do
      test -f $path && ( mkdir -p $out/$(dirname $path) ; cp -r $path $out/$path )
      test -d $path && ( mkdir -p $out/$path ; cp -r $path/* $out/$path )
    done
  ''
