{ mkDerivation, base, free, mtl, stdenv, text, transformers }:
mkDerivation {
  pname = "free-experiments";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base free mtl text transformers ];
  homepage = "https://github.com/dhess/free-experiments/";
  description = "Playing with the Free monad";
  license = stdenv.lib.licenses.bsd3;
}
