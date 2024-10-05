{ ripgrep
, runCommand
, self
, lib
}:
{ allowed ? []
}:
let
  allowed' = lib.pipe allowed [
    (map (lib.replaceStrings ["."] ["\."]))
    (lib.concatStringsSep "|")
  ];
in
runCommand "with-lib-check.sh" {
  nativeBuildInputs = [
    ripgrep
  ];
} ''
  [ "$(rg "with lib;" -n ${self} | rg -v "${allowed'}" | tee with-lib.rg | wc -l)" -le 1 ] \
    || ( cat with-lib.rg ; exit 1 ) \
    && exit 0
''
