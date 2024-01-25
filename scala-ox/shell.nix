{ pkgs ? import <nixpkgs> {} }:

  pkgs.mkShell {
    buildInputs = [
      pkgs.jdk21
    ];
  }
