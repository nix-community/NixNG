{
  stdenv,
  fetchFromGitHub,
  gnumake,
  gnum4,
}:
stdenv.mkDerivation (self: {
  pname = "dinit";
  version = "0.21.0";

  src = fetchFromGitHub {
    owner = "davmac314";
    repo = "dinit";
    rev = "v" + self.version;
    hash = "sha256-3OsuKA34xcanDM72mu0K97Z4lngWZbWOoJzeedPWQDs=";
  };

  patches = [
    ./0001-Revert-Resolve-environment-file-using-openat.patch
    ./dinit-allow-multiple-env-files.patch
  ];

  nativeBuildInputs = [
    gnumake
    gnum4
  ];

  makeFlags = [
    "DESTDIR=/"
    "SBINDIR=\${out}/bin"
  ];

  meta = {
    mainProgram = "dinit";
  };
})
