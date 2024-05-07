with import <nixpkgs> {};
mkShell {
  buildInputs = [
    pkgs.nodejs_21
    pkgs.vscode
  ];
}
