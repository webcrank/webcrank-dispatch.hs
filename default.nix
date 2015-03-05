{ mkDerivation, base, bytestring, doctest, QuickCheck, reroute
, stdenv, text
}:
mkDerivation {
  pname = "webcrank-dispatch";
  version = "0.1";
  src = ./.;
  buildDepends = [ base bytestring reroute text ];
  testDepends = [ base doctest QuickCheck ];
  license = stdenv.lib.licenses.unfree;
}
