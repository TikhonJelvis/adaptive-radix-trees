{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation
      , array
      , base
      , binary
      , bytestring
      , containers
      , criterion
      , deepseq
      , HUnit
      , primitive
      , QuickCheck
      , stdenv
      , tasty
      , tasty-quickcheck
      }:
      mkDerivation {
        pname = "adaptive-radix-trees";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          array
          base
          binary
          bytestring
          containers
          criterion
          deepseq
          primitive
        ];
        testHaskellDepends = [
          base
          bytestring
          HUnit
          QuickCheck
          tasty
          tasty-quickcheck
        ];
        enableLibraryProfiling = true;
        enableExecutableProfiling = true;
        description = "Efficient integer maps based on adaptive radix trees";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
