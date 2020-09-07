let
  pkgs = import <nixpkgs> { };

in
  { server = pkgs.haskellPackages.callPackage ./server.nix { };
  }
