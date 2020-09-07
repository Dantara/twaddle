let
  pkgs = import <nixpkgs> { };

in
  { common = pkgs.haskellPackages.callPackage ./common.nix { };
  }
