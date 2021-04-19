with import <nixpkgs> {};
mkShell {
  name = "env";
  nativeBuildInputs = [ cmake ];
  buildInputs = [ boehmgc ];
}
