{ pkgs ? (import <nixpkgs> {}) }:

with pkgs;
stdenv.mkDerivation {
  name = "Scheme-Runtime";
  nativeBuildInputs = [ cmake ];
  buildInputs = [ boehmgc ];
  src = fetchFromGitHub { owner = "teichholz";
                          repo = "Schemer-Runtime";
                          rev = "e3e8239";
                          sha256= "sha256-OPd2YVHy+VmxP7nm72pkfdO+Ms47L4HIjSXEreMuXAY="; };

  installPhase = ''
    mkdir -p $out/lib
    cp libRuntime.a $out/lib
    cp libRuntime_s.so $out/lib
  '';

  dontPatch = true;
  doCheck = false;
}
