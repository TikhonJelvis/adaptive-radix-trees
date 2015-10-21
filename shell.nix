{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, bytestring, binary, containers, criterion, deepseq, HUnit, primitive
      , QuickCheck, stdenv, tasty, tasty-quickcheck, vector
      , vector-algorithms
      }:
      mkDerivation {
        pname = "adaptive-radix-trees";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          base bytestring binary containers criterion deepseq primitive vector vector-algorithms
        ];
        testHaskellDepends = [
          base bytestring HUnit QuickCheck tasty tasty-quickcheck
        ];
        description = "Efficient integer maps based on adaptive radix trees";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
