{ mkDerivation, base, classy-prelude, containers, stdenv, text
, unordered-containers, cabal-install, data-fix, system-filepath
, hspec, QuickCheck
}:
mkDerivation {
  pname = "nix-eval";
  version = "0.0.0";
  src = ./.;
  testDepends = [hspec];
  buildDepends = [
    base classy-prelude containers text unordered-containers cabal-install
    data-fix system-filepath QuickCheck
  ];
  license = stdenv.lib.licenses.mit;
}
