{ mkDerivation, base, mtl, stdenv, text, webcrank-dispatch }:
mkDerivation {
  pname = "webcrank-dispatch-examples";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  buildDepends = [ base mtl text webcrank-dispatch ];
  license = stdenv.lib.licenses.bsd3;
}
