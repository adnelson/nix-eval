{ mkDerivation, base, classy-prelude, containers, stdenv, text
, unordered-containers, cabal-install
}:
mkDerivation {
  pname = "nix-eval";
  version = "0.0.0";
  src = ./.;
  buildDepends = [
    base classy-prelude containers text unordered-containers cabal-install
  ];
  license = stdenv.lib.licenses.mit;
}
