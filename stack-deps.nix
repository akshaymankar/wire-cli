let
  pkgs = import ./nix;
in
pkgs.haskell.lib.buildStackProject {
  name = "wire-server";
  buildInputs = with pkgs; [
    icu
    pkgconfig
  ];
  ghc = pkgs.haskell.compiler.ghc865;
}
