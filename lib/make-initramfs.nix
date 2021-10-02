{ system
, runCommandNoCC
, findutils
, cpio
, gzip
, path
, name
}:
runCommandNoCC name
{
  nativeBuildInputs = [
    findutils
    cpio
    gzip
  ];
}
  ''
    ( cd ${path} ; find . | cpio -o -H newc --quiet | gzip -9 ) > $out
  ''
