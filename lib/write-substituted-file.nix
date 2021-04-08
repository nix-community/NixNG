{ runCommandNoCCLocal
, runtimeShell
, busybox
}:
{
  name
, file
, substitutes
}:
runCommandNoCCLocal name ({
  nativeBuildInputs = [ busybox ];
} // substitutes)
  ''
    TMPFILE=$(mktemp)
    substituteAll ${file} $TMPFILE
    touch $out
    cat $TMPFILE >> $out
  ''
