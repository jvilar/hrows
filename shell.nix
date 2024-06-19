{ pkgs ? import <nixpkgs> {} }:

with pkgs;

mkShell {
  buildInputs = [
    (haskell-language-server.override { supportedGhcVersions = ["96"]; })
  ];
}


