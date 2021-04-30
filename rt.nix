{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;
stdenv.mkDerivation {
  name = "Scheme-Runtime";
  nativeBuildInputs = [ cmake ];
  buildInputs = [ boehmgc ];
  src = fetchFromGitHub { owner = "teichholz";
                          repo = "Schemer-Runtime";
                          rev = "main";
                          sha256= "sha256-TzhltbySQAFw5uhHWvaN9L/4KeLzhKdEsHXczOZr1fA="; };

  installPhase = ''
    mkdir -p $out/lib
    cp libRuntime.a $out/lib
    cp libRuntime_s.so $out/lib
  '';

  dontPatch = true;
  doCheck = false;
}
