{
  description = "A very basic flake";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = {nixpkgs, flake-utils, ...}:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [(import ./nix/overlays/wire-cli.nix)];
        };
      in rec {
        packages.dev-env = pkgs.buildEnv {
          name = "wire-clie-build-env";
          paths = [
            # Tools
            pkgs.haskell.compiler.ghc8107
            pkgs.haskellPackages.cabal-install-head
            pkgs.gnumake
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.cabal-plan

            # For cabal
            pkgs.pkgconfig
            pkgs.binutils

            # Core
            pkgs.cryptobox
            pkgs.openssl.dev
            pkgs.openssl.out
            pkgs.zlib.dev
            pkgs.zlib.out

            # For wire-api
            pkgs.lzma.dev
            pkgs.lzma.out

            # hsaml2
            pkgs.libxml2.dev

            # Message
            pkgs.protobuf
            pkgs.haskellPackages.proto-lens-protoc

            # GUI
            pkgs.pcre.dev
            pkgs.libffi.dev
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
        };
        defaultPackage = packages.dev-env;
    });
}
