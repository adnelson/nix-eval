{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
let
  haskellPackages = nixpkgs.pkgs.haskell.packages.${compiler};
in

haskellPackages.callPackage ./project.nix {
  hnix = haskellPackages.callPackage ../hnix/project.nix {};
}
