# SPDX-FileCopyrightText: 2026 Richard Brežák <magic_rb@redalder.org> and NixNG contributors
#
# SPDX-License-Identifier: MPL-2.0

{
  stdenv,
  fetchFromGitHub,
  gnumake,
  gnum4,
}:
stdenv.mkDerivation (self: {
  pname = "dinit";
  version = "0.20.0";

  src = fetchFromGitHub {
    owner = "davmac314";
    repo = "dinit";
    rev = "v" + self.version;
    hash = "sha256-/slWdU2p1iOmE2KFsrZKpgApadAzofDXvJVah0AMRq8=";
  };

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
