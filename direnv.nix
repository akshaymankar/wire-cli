{ ghc ? "ghc8104" }:
let
  pkgs = import ./nix;

  # hls = pkgs.haskell-language-server.override { supportedGhcVersions = ["8104"]; };
  # hls = pkgs.haskellPackages.ghcide;
  hls = pkgs.haskellPackages.haskell-language-server;

  deps = [
      # Tools
      pkgs.haskell.compiler.${ghc}
      pkgs.haskellPackages.cabal-install
      pkgs.gnumake

      # For cabal
      pkgs.pkgconfig
      pkgs.binutils

      # Core
      pkgs.cryptobox
      pkgs.openssl.dev
      pkgs.openssl.out
      pkgs.zlib.dev
      pkgs.zlib.out

      # Message
      pkgs.protobuf

      # GUI
      pkgs.gtk4.dev
      pkgs.gtk4.out
      pkgs.gobject-introspection.dev
      pkgs.glib.dev
      pkgs.pango.dev
      pkgs.pango.out
      pkgs.graphene
      pkgs.gdk-pixbuf.dev
      pkgs.gdk-pixbuf.out
      pkgs.cairo.dev
      pkgs.atk.dev
      pkgs.atk.out
      pkgs.harfbuzz.dev
      pkgs.harfbuzz.out
      pkgs.vulkan-loader.dev
  ];
  devDeps = deps ++ [ hls ];
in {
  devEnv = pkgs.buildEnv {
    name = "wire-cli";
    paths = devDeps;
  };
  direnv = pkgs.buildEnv { name ="direnv"; paths = [pkgs.direnv]; };
}

