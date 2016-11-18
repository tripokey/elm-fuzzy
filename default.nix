with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/64ec4dd87bf7b211773541fa350ef2f56b9c658f.tar.gz) {};

stdenv.mkDerivation {
  name = "elm-fuzzy";

  src = ./.;

  buildInputs = [ elmPackages.elm ];

  buildPhase = ''
    cd demo
    HOME=$PWD elm-make --yes Demo.elm
    '';

  installPhase = ''
    mkdir -p $out
    cp index.html $out/
    '';
}
