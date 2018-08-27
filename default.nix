with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/aa8bdaf0c4ed67acfab6a1977b70c356bbf0543a.tar.gz) {};

stdenv.mkDerivation {
  name = "elm-fuzzy";

  src = ./.;

  buildInputs = [ elmPackages.elm ];

  buildPhase = ''
    cd demo
    HOME=$PWD elm make Demo.elm
    '';

  installPhase = ''
    mkdir -p $out
    cp index.html $out/
    '';
}
