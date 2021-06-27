{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;
stdenv.mkDerivation {
  name = "Scheme-Runtime";
  nativeBuildInputs = [ cmake ];
  buildInputs = [ boehmgc ];
  src = fetchFromGitHub { owner = "teichholz";
                          repo = "Schemer-Runtime";
                          rev = "70c83a8";
                          sha256 = "sha256-pQIdbvn0MhiX5BY9MEqCMs7+NRSGBS3/oVObKOmumcU="; };

  installPhase = ''
    mkdir -p $out/lib
    cp libRuntime.a $out/lib
    cp libRuntime_s.so $out/lib
  '';
}
