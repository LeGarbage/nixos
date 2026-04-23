{ stdenv, fetchzip, ... }:
stdenv.mkDerivation (finalAttrs: rec {
  pname = "libtexprintf";
  version = "1.31";

  src = fetchzip {
    url = "https://github.com/bartp5/libtexprintf/releases/download/v${version}/libtexprintf-${version}.tar.gz";
    hash = "sha256-SmMTGCX4eCGAWC5Ej5Ke+9GOK6QyZ7OqDHsF9Tyiq4U=";
  };
})
