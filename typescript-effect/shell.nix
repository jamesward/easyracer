with import <nixpkgs> {};
mkShell {
  buildInputs = [
    pkgs.nodejs_23
    pkgs.vscode
  ];
}
