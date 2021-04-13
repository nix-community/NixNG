{ stdenv, lib, fetchurl }:
stdenv.mkDerivation {
  name = "cronie";
  version = "1.5.7";

  configureFlags = "--localstatedir=/var --sysconfdir=/etc";

  src = fetchurl {
    url = "https://github.com/cronie-crond/cronie/releases/download/cronie-1.5.7/cronie-1.5.7.tar.gz";
    sha256 = "sha256-U4vPry6Yblrh7fbRRyp36oJx1qkAWu4kl6ntbhMyDrM=";
  };
}
