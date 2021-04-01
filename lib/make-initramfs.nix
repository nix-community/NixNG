{ system
, runCommandNoCC 
, findutils, cpio, gzip
}:
{ path, name
, compress ? true
}:
runCommandNoCC name
  { nativeBuildInputs = [
      findutils
      cpio
      gzip
    ];
  }
  (if compress then
  ''
    ( cd ${path} ; find . | cpio -o -H newc --quiet | gzip -9 ) > $out
  ''
  else
  ''
    ( cd ${path} ; find . | cpio -o -H newc --quiet ) > $out
  '')
