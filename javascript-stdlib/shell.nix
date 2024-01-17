with import <nixpkgs> {};
mkShell {
  buildInputs = [
    pkgs.nodejs_21
  ];
}
