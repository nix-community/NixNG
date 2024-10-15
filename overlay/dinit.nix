{
  stdenv,
  fetchFromGitHub,
  gnumake,
  gnum4,
}:
stdenv.mkDerivation (self: {
  pname = "dinit";
  version = "0.19.0";

  src = fetchFromGitHub {
    owner = "davmac314";
    repo = "dinit";
    rev = "v" + self.version;
    hash = "sha256-ApB0pEFSyawNASF/rqRmhT4FLofZzYmNdNmG2FGpnnk=";
  };

  patches = [
    ./dinit-dont-shutdown-system.patch
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
