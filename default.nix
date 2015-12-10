{ nixpkgs ? import <nixpkgs> {}
, compiler ? null
, profiling ? false
}:

let
  hpkgs =
    if compiler == null
      then nixpkgs.pkgs.haskellPackages
      else nixpkgs.pkgs.haskell.packages.${compiler};
in
  (hpkgs.override {
    overrides = self: super: {
      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = profiling;
      });
    };
  }).callPackage ./webcrank-dispatch.nix { }
