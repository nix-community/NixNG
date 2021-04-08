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
    mkdir -p $out/bin && touch $out/bin/${name}
    echo "#! ${runtimeShell}" > $out/bin/${name}
    cat $TMPFILE >> $out/bin/${name}
    chmod +x $out/bin/${name}
  ''
