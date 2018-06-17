let
  rev = "cd960b965f2587efbe41061a4dfa10fc72a28781";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "0k2pk3y54kh2lz8yaj2438sqclayhsc0b2w262qb6iwyafby8pr0";
  };
  nixpkgs = import pkgs {
    config = {
      packageOverrides = pkgs_: with pkgs_; {
        haskell = haskell // {
          packages = haskell.packages // {
            ghc842-profiling = haskell.packages.ghc842.override {
              overrides = self: super: {
                mkDerivation = args: super.mkDerivation (args // {
                  enableLibraryProfiling = true;
                });
              };
            };
          };
        };
      };
    };
  };
in { compiler ? "ghc842", ci ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, annotated-wl-pprint, base, containers
      , haskeline, mtl, parsec, stdenv
      }:
      mkDerivation {
        pname = "dep";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          annotated-wl-pprint base containers haskeline mtl
          parsec
        ];
        description = "A very small implementation of the Calculus of Constructions for experimentation purposes";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
