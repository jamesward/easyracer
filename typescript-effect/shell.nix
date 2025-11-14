with import <nixpkgs> {};
mkShell {
  buildInputs = [
    pkgs.nodejs
#    pkgs.vscode
  ];
}
