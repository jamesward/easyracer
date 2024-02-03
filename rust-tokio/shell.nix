let
  pkgs =
    import <nixpkgs> {};
  rust-toolchain = pkgs.symlinkJoin {
    name = "rust-toolchain";
    paths = [ pkgs.rustc pkgs.cargo pkgs.rustfmt ];
  };
in with pkgs;
mkShell {
  name = "scriptr";
  buildInputs = [rust-toolchain];
  nativeBuildInputs = with pkgs; [ gcc pkg-config openssl ];
  RUST_BACKTRACE = 1;
}
