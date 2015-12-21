{ mkDerivation, base, classy-prelude, containers, stdenv, text
, unordered-containers, cabal-install, data-fix, system-filepath
}:
mkDerivation {
  pname = "nix-eval";
  version = "0.0.0";
  src = ./.;
  buildDepends = [
    base classy-prelude containers text unordered-containers cabal-install
    data-fix system-filepath
  ];
  license = stdenv.lib.licenses.mit;
}
