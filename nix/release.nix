let

  pkgs = import ./nixpkgs.nix ;
  src = ../. ;

  server = pkgs.haskell.packages.ghc.callCabal2nix "twaddle" ../. {};
  client = pkgs.haskell.packages.ghcjs.callCabal2nix "twaddle" ../. {};

in

  pkgs.runCommand "twaddle" { inherit client server; } ''
    mkdir -p $out/{bin,static}
    cp ${server}/bin/* $out/bin
    cp ${client}/bin/client.jsexe/all.js $out/static
    cp -rf ${src}/assets/* $out/static/
  ''

