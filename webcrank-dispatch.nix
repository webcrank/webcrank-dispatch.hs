{ mkDerivation, base, hvect, mtl, path-pieces, reroute, stdenv
, text, unordered-containers
}:
mkDerivation {
  pname = "webcrank-dispatch";
  version = "0.2";
  src = builtins.filterSource (path: type: baseNameOf path != "dist" && baseNameOf path != ".hdevtools.sock") ./.;
  libraryHaskellDepends = [
    base hvect mtl path-pieces reroute text unordered-containers
  ];
  homepage = "https://github.com/webcrank/webcrank-dispatch.hs";
  description = "A simple request dispatcher";
  license = stdenv.lib.licenses.bsd3;
}
