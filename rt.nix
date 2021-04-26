{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;
stdenv.mkDerivation {
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
}
