let

  pkgs = import ./nixpkgs.nix ;

  server = pkgs.haskell.packages.ghc.callCabal2nix "twaddle" ../. {};
  client = pkgs.haskell.packages.ghcjs.callCabal2nix "twaddle" ../. {};

in {
  inherit pkgs;
  inherit server;
  inherit client;
  miso-ghcjs = pkgs.haskell.packages.ghcjs.miso;
  miso-ghc = pkgs.haskell.packages.ghc.miso;
}

