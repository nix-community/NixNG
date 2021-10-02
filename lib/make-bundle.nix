{ system
, runCommandNoCC
, writeReferencesToFile
, path
, name
}:

runCommandNoCC name
{ }
  ''
    set -o pipefail
    mkdir -p $out
    xargs tar c < ${writeReferencesToFile path} | tar -xC $out/

    ls -lah $out/nix/store
  ''
