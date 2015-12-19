{ mkDerivation, base, classy-prelude, containers, stdenv, text
, unordered-containers, cabal-install, data-fix
}:
mkDerivation {
  pname = "nix-eval";
  version = "0.0.0";
  src = ./.;
  buildDepends = [
    base classy-prelude containers text unordered-containers cabal-install
    data-fix
  ];
  license = stdenv.lib.licenses.mit;
}
