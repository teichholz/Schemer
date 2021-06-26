{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;
stdenv.mkDerivation {
  name = "Scheme-Runtime";
  nativeBuildInputs = [ cmake ];
  buildInputs = [ boehmgc ];
  src = fetchFromGitHub { owner = "teichholz";
                          repo = "Schemer-Runtime";
                          rev = "163ea02";
                          sha256= "sha256-mwpoe/r+83K3to97Q4T4s7+4RkW9SevZB+FvYVS6PEc="; };

  installPhase = ''
    mkdir -p $out/lib
    cp libRuntime.a $out/lib
    cp libRuntime_s.so $out/lib
  '';

  dontPatch = true;
  doCheck = false;
}
