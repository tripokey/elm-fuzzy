with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/a24728fe295f28648f1812b3565541f7ef6269f1.tar.gz) {};

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
