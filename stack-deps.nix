let
  pkgs = import ./nix;
in
pkgs.haskell.lib.buildStackProject {
  name = "wire-cli";
  buildInputs = with pkgs; [
    cryptobox
    icu
    libsodium
    openssl
    pkgconfig
    zlib
  ];
  ghc = pkgs.haskell.compiler.ghc883;
}
