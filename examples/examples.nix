with (import <nixpkgs> {}).pkgs;
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs; });

let modifiedHaskellPackages = haskellngPackages.override {
    overrides = self: super: {
      webcrank-dispatch = self.callPackage ../. {};
      examples = self.callPackage ./. {};
    };
  };
in modifiedHaskellPackages.examples
