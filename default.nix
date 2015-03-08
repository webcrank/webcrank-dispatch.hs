{ mkDerivation, base, bytestring, mtl, path-pieces, reroute, stdenv
, text, unordered-containers
}:
mkDerivation {
  pname = "webcrank-dispatch";
  version = "0.2";
  src = ./.;
  buildDepends = [
    base bytestring mtl path-pieces reroute text unordered-containers
  ];
  homepage = "https://github.com/webcrank/webcrank-dispatch.hs";
  description = "A simple request dispatcher";
  license = stdenv.lib.licenses.bsd3;
}
