{ghc}:

with (import <nixpkgs> {});
let rt = stdenv.mkDerivation {
  name = "Scheme-Runtime";
  nativeBuildInputs = [ cmake ];
  buildInputs = [ boehmgc ];
  src = fetchTarball { url = "https://github.com/teichholz/Schemer-Runtime/archive/refs/heads/main.tar.gz"; };

  installPhase = ''
    mkdir -p $out/lib
    cp libRuntime.a $out/lib
    cp libRuntime_s.so $out/lib
  '';

  dontPatch = true;
  doCheck = false;
}; in

haskell.lib.buildStackProject {
  name = "Env";
  inherit ghc;
  buildInputs = [ llvm_9 boehmgc rt ];
}
