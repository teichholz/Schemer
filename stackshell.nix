{ghc}:

with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  name = "Env";
  inherit ghc;
  buildInputs = [ llvm_9 boehmgc # (import ./rt.nix {})
                ];
}
