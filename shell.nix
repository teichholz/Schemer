{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [ stack boehmgc (import ./rt.nix {}) ];
}
