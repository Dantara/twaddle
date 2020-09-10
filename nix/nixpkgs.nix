let

  bootstrap = import <nixpkgs> {};

  nixpkgs-src = bootstrap.fetchFromGitHub {
    owner = "NixOS";
    repo  = "nixpkgs";
    rev = "1c92cdaf7414261b4a0e0753ca9383137e6fef06";
    sha256 = "0d3fqa1aqralj290n007j477av54knc48y1bf1wrzzjic99cykwh";
  };

  ghc-version = "ghc865";
  ghcjs-version = "ghcjs86";

  miso-src = bootstrap.fetchFromGitHub {
    owner = "dmjio";
    repo  = "miso";
    rev = "1.7.1";
    sha256 = "0q44lxzz8pp89ccaiw3iwczha8x2rxjwmgzkxj8cxm97ymsm0diy";
  };

  jsaddle-warp-src = bootstrap.fetchFromGitHub {
    owner = "ghcjs";
    repo  = "jsaddle";
    rev = "jsaddle-warp-0.9.5.0";
    sha256 = "068xdkyq3xdl212p9b6fkf7sajdg7n0s42mmqccvvg0n024nc3a8";
  };
  
   servant-src = bootstrap.fetchFromGitHub { 
    owner = "haskell-servant";
    repo = "servant";
    rev = "v0.17" ;
    sha256 = "1yzxkz2yssw20ll419lh4b27abi5mak22crfjmpkzlhx2a3fwya9";
  };

  config = { 

    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {

          ghc = pkgs.haskell.packages.${ghc-version}.override {
            overrides = self: super: with pkgs.haskell.lib; {

              miso = self.callCabal2nix "miso" miso-src {};

            };
          };

          } // {

          ghcjs = pkgs.haskell.packages.${ghcjs-version}.override {
            overrides = self: super: with pkgs.haskell.lib; {

              network = dontCheck (doJailbreak super.network_2_6_3_1);
              servant-client = dontCheck (doJailbreak super.servant-client);
              servant = dontCheck (doJailbreak super.servant);
              semigroupoids = dontCheck (doJailbreak super.semigroupoids);
              http-types = dontCheck (doJailbreak super.http-types);
              comonad = dontCheck (doJailbreak super.comonad);
              memory = dontCheck (doJailbreak super.memory);
              QuickCheck = dontCheck (doJailbreak super.QuickCheck);
              tasty = dontCheck (doJailbreak super.tasty);
              tasty-golden = dontCheck (doJailbreak super.tasty-golden);
              tasty-quickcheck = dontCheck (doJailbreak super.tasty-quickcheck);
              aeson = dontCheck (doJailbreak super.aeson);
              cookie = dontCheck (doJailbreak super.cookie);
              scientific = dontCheck (doJailbreak super.scientific);
              time-compat = dontCheck (doJailbreak super.time-compat);
              uuid-types = dontCheck (doJailbreak super.uuid-types);
              streaming-commons = dontCheck (doJailbreak super.streaming-commons);

              miso = self.callCabal2nix "miso" miso-src {};
              jsaddle-warp = self.callCabal2nix "jsaddle-warp" "${jsaddle-warp-src}/jsaddle-warp" {};

              servant-client-ghcjs = (self.callCabal2nix "servant-client-ghcjs" "${servant-src}/servant-client-ghcjs" {}).overrideDerivation (attrs: {
                preConfigure = ''
                  sed -i 's/4.12/5/' servant-client-ghcjs.cabal
                '';
              });

            };
          };

        };
      };
    };
  };

in import nixpkgs-src { inherit config; }

