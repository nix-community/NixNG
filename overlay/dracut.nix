{
  stdenv,
  fetchFromGitHub,
  lib,
  pkg-config,
  kmod,
  makeWrapper,
  utillinux
  # asciidoc,
  # libxslt
}:
let
  version = "057";
in
stdenv.mkDerivation {
  pname = "dracut";
  inherit version;
  src = fetchFromGitHub {
    repo = "dracut";
    owner = "dracutdevs";
    rev = version;
    sha256 = "sha256-stbRnbRudJdXMX32mS8Fz3cL8TUwFX13l+59ai0NTiE=";
  };

  buildInputs = [
    kmod
    makeWrapper
  ];

  configureFlags = [
    "--disable-documentation"
  ];

  nativeBuildInputs = [
    pkg-config
    # asciidoc
    # libxslt
  ];

  patchPhase =
    ''
      patchShebangs configure
      sed -i 's~srcmods="$dracutsysrootdir/lib/modules/$kernel/"~srcmods="/run/current-system/kernel-modules"~' dracut-init.sh
    '';

  postInstall =
    ''
      for binary in $out/bin/*
      do
        wrapProgram "$binary" \
          --prefix PATH : "${utillinux}/bin" \
          --add-flags "-r $out --tmpdir /tmp" \
          --set dracutbasedir "$out/lib/dracut" \
          --run 'export DRACUT_PATH="$PATH"'
      done
    '';
}
