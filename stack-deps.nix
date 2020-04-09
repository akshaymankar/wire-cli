let
  pkgs = import ./nix;
in
pkgs.haskell.lib.buildStackProject {
  name = "wire-server";
  buildInputs = with pkgs; [
    icu
    pkgconfig
    zlib
  ];
  ghc = pkgs.haskell.compiler.ghc865;
}
