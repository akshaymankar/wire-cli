let
  pkgs = import ./nix;
in
with pkgs; mkShell {
  name = "wire-cli";
  buildInputs = [
    libsodium
    cryptobox
    openssl
    pkgconfig
    zlib
    haskell.compiler.ghc884
    haskellPackages.cabal-install
    ncurses
    git
  ];
  shellHook = ''
    export LD_LIBRARY_PATH=${ncurses}/lib:${openssl.out}/lib:${zlib}/lib
    '';
}
