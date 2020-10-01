{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, adjunctions, base, comonad, distributive
      , profunctors, stdenv
      }:
      mkDerivation {
        pname = "higher-functors";
        version = "0.1.0.0";
        src = ./.;
        libraryHaskellDepends = [
          adjunctions base comonad distributive profunctors
        ];
        testHaskellDepends = [ base ];
        homepage = "https://github.com/emilypi/higher-functors";
        description = "Higher-order functors";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
