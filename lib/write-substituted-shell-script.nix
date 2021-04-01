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
    echo "#! ${runtimeShell}" > $out
    cat $TMPFILE >> $out
    chmod +x $out
  ''
