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

        c-lib-out-deps = [
            # Core
            pkgs.cryptobox
            pkgs.openssl.out
            pkgs.zlib.out

            # For wire-api
            pkgs.lzma.out

            # GUI
            pkgs.gtk4.out
            pkgs.pango.out
            pkgs.graphene
            pkgs.gdk-pixbuf.out
            pkgs.atk.out
            pkgs.harfbuzz.out
        ];

        compile-deps = pkgs.buildEnv {
          name = "wire-server-compile-deps";
          paths = [
            pkgs.bash
            pkgs.coreutils
            pkgs.gnused
            pkgs.gnugrep
            pkgs.pkgconfig
            pkgs.gawk
            pkgs.git

            pkgs.haskell.compiler.ghc8107

            # For cabal
            pkgs.pkgconfig
            pkgs.binutils

            # Core
            pkgs.openssl.dev
            pkgs.zlib.dev

            # For wire-api
            pkgs.lzma.dev

            # hsaml2
            pkgs.libxml2.dev

            # Message
            pkgs.protobuf
            pkgs.haskellPackages.proto-lens-protoc

            # GUI
            pkgs.pcre.dev
            pkgs.libffi.dev
            pkgs.gobject-introspection.dev
            pkgs.glib.dev
            pkgs.pango.dev
            pkgs.gtk4.dev
            pkgs.gdk-pixbuf.dev
            pkgs.cairo.dev
            pkgs.atk.dev
            pkgs.harfbuzz.dev
            pkgs.vulkan-loader.dev
          ] ++ c-lib-out-deps;
        };

        # This performs roughly the same setup as direnv's load_prefix function, but
        # only when invoking cabal. This means that we can set LD_LIBRARY_PATH just
        # for cabal, as setting it in direnv can interfere with programs in the host
        # system, especially for non-NixOS users.
        cabal-wrapper = pkgs.writeShellScriptBin "cabal" ''
          export CPATH="${compile-deps}/include"
          export LD_LIBRARY_PATH="${compile-deps}/lib"
          export LIBRARY_PATH="${compile-deps}/lib"
          export PKG_CONFIG_PATH="${compile-deps}/lib/pkgconfig"
          export PATH="${compile-deps}/bin"
          export CONFIG_SHELL="${compile-deps}/bin/sh"

          export XDG_DATA_DIRS="${compile-deps}/share"
          export GI_TYPELIB_PATH="${compile-deps}/lib/girepository-1.0"

          exec "${pkgs.haskellPackages.cabal-install-head}/bin/cabal" "$@"
        '';

      in rec {
        packages.dev-env = pkgs.buildEnv {
          name = "wire-clie-build-env";
          paths = [
            # Tools
            pkgs.haskell.compiler.ghc8107
            cabal-wrapper
            pkgs.gnumake
            pkgs.haskellPackages.haskell-language-server
            pkgs.haskellPackages.cabal-plan
            pkgs.jq
            pkgs.haskellPackages.hspec-discover # So, HLS can find it.

            # cabal2nix
            pkgs.cabal2nix
            pkgs.yq
          ] ++ c-lib-out-deps;
        };
        packages.wire-cli =
          let hlib = pkgs.haskell.lib;
              haskellPackages = pkgs.haskell.packages.ghc8107.override {
                overrides = hself: hsuper:
                  let generated = import ./nix/haskell-overrides/overrides.nix hself hsuper;
                      manual = {
                        wire-message =
                          let basic = hself.callPackage ./wire-message/default.nix {};
                          in hlib.addBuildTool basic pkgs.protobuf;
                        wire-cli = hself.callPackage ./default.nix {};
                        network-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.network-arbitrary);
                        polysemy-zoo = hlib.markUnbroken (hlib.doJailbreak hsuper.polysemy-zoo);
                        cql = hlib.markUnbroken hsuper.cql;
                        cql-io = hlib.dontCheck hsuper.cql-io;
                        lens-datetime = hlib.markUnbroken (hlib.doJailbreak hsuper.lens-datetime);
                        wai-predicates = hlib.markUnbroken hsuper.wai-predicates;
                        gi-gio-hs-list-model = hlib.markUnbroken hsuper.gi-gio-hs-list-model;
                        http-client-websockets = hlib.markUnbroken (hlib.dontCheck hsuper.http-client-websockets);
                        polysemy = hsuper.polysemy_1_7_1_0;
                        polysemy-plugin = hsuper.polysemy-plugin_0_4_3_0;
                        hsaml2 = hlib.dontCheck generated.hsaml2;
                        universe-base = hlib.addExtraLibrary hsuper.universe-base hsuper.OneTuple;
                        wire-message-proto-lens = hlib.addBuildTool generated.wire-message-proto-lens pkgs.protobuf;
                        hscim = hlib.doJailbreak generated.hscim;
                        bytestring-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.bytestring-arbitrary);
                        servant-server = hlib.dontCheck hsuper.servant-server;
                        singeltons = hlib.doJailbreak generated.singletons;
                        saml2-web-sso = hlib.dontCheck generated.saml2-web-sso;
                      };
                  in generated // manual;
              };
          in haskellPackages.wire-cli;
        defaultPackage = packages.dev-env;
    });
}
