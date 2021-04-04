{ ghc ? "ghc8104" }:
let
  pkgs = import ./nix;

  deps = [
      pkgs.haskell.compiler.${ghc}
      pkgs.haskellPackages.cabal-install

      pkgs.pkgconfig
      pkgs.binutils

      pkgs.cryptobox
      pkgs.openssl.dev
      pkgs.openssl.out
      pkgs.zlib.dev
      pkgs.zlib.out

      pkgs.protobuf
    ];
  devDeps = deps ++ [pkgs.haskell-language-server pkgs.hlint];
in {
  devEnv = pkgs.buildEnv {
    name = "wire-cli";
    paths = devDeps;
  };
  direnv = pkgs.buildEnv { name ="direnv"; paths = [pkgs.direnv]; };
}

