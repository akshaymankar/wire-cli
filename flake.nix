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
        hlib = pkgs.haskell.lib;
        # Avoids unnecessary recompiles
        filteredSource = pkgs.lib.cleanSourceWith {
          src = ./.;
          filter = path: type:
            let baseName = baseNameOf (toString path);
            in pkgs.lib.cleanSourceFilter path type && !(
              baseName == "wire-message" ||
              baseName == "flake.nix" ||
              baseName == "flake.lock" ||
              baseName == "dist-newstyle" ||
              baseName == "hack" ||
              baseName == "nix" ||
              baseName == ".hls-env" ||
              baseName == "Makefile" ||
              builtins.match "^cabal\.project\..*$" baseName != null ||
              baseName == "lsp-wrapper.sh" ||
              baseName == "hack" ||
              baseName == ".envrc" ||
              baseName == "hie.yaml" ||
              baseName == ".hlint.yaml" ||
              baseName == ".hspec" ||
              baseName == "pins.yaml" ||
              baseName == "ci"
            );
        };
        haskellPackages = pkgs.haskell.packages.ghc8107.override {
          overrides = hself: hsuper:
            let generated = import ./nix/haskell-overrides/overrides.nix hself hsuper;
                manual = {
                  wire-message =
                    let basic = hself.callPackage ./wire-message/default.nix {};
                    in hlib.addBuildTool basic pkgs.protobuf;
                  wire-cli = hlib.overrideSrc (hself.callPackage ./default.nix {}) {src = filteredSource;};
                  network-arbitrary = hlib.markUnbroken (hlib.doJailbreak hsuper.network-arbitrary);
                  polysemy-zoo = hlib.markUnbroken (hlib.doJailbreak hsuper.polysemy-zoo);
                  cql = hlib.markUnbroken hsuper.cql;
                  cql-io = hlib.dontCheck hsuper.cql-io;
                  lens-datetime = hlib.markUnbroken (hlib.doJailbreak hsuper.lens-datetime);
                  wai-predicates = hlib.markUnbroken hsuper.wai-predicates;
                  gi-gio-hs-list-model = hlib.markUnbroken hsuper.gi-gio-hs-list-model;
                  gi-gtk = hsuper.gi-gtk_4_0_5;
                  gi-gdk = hsuper.gi-gdk_4_0_4;
                  gi-graphene = hlib.markUnbroken (hlib.addPkgconfigDepend hsuper.gi-graphene pkgs.graphene);
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
        wire-cli = hlib.justStaticExecutables haskellPackages.wire-cli;

        # Docker tools doesn't create tmp directories but some processes need this
        # and so we have to create it ourself.
        tmpDir = pkgs.runCommand "tmp-dir" { } ''
          mkdir -p $out/tmp
          mkdir -p $out/var/tmp
        '';
      in rec {
        packages = {
          inherit wire-cli;
          docker-image = pkgs.dockerTools.streamLayeredImage {
            name = "registry.git.coop/akshay/wire-cli";
            maxLayers = 20;
            contents = [
              tmpDir
              pkgs.cacert
              pkgs.iana-etc
              pkgs.dockerTools.fakeNss
              pkgs.dockerTools.usrBinEnv
              pkgs.bashInteractive
              pkgs.gnugrep
              pkgs.coreutils
              pkgs.dig
              pkgs.curl
              pkgs.less
              pkgs.gnutar
              pkgs.gzip
              pkgs.openssl
              pkgs.which
              pkgs.unixtools.procps
              wire-cli
            ];
            # Any mkdir running in this step won't actually make it to the image,
            # hence we use the tmpDir derivation in the contents
            fakeRootCommands = ''
              chmod 1777 tmp
              chmod 1777 var/tmp
            '';
            config = {
              Entrypoint = [ "${pkgs.dumb-init}/bin/dumb-init" "--" "${wire-cli}/bin/wire-cli" ];
              Env = [
                "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
                "LOCALE_ARCHIVE=${pkgs.glibcLocales}/lib/locale/locale-archive"
                "LANG=en_GB.UTF-8"
              ];
              User = "65534";
            };
          };
          dev-shell = haskellPackages.shellFor {
            packages = p: [p.wire-cli p.wire-message];
            buildInputs = [
              # Tools
              pkgs.cabal-install
              pkgs.gnumake
              pkgs.haskell.packages.ghc8107.haskell-language-server
              pkgs.haskellPackages.cabal-plan
              pkgs.jq

              # cabal2nix
              pkgs.cabal2nix
              pkgs.yq

              # concourse
              pkgs.fly
              pkgs.dhall-json
            ];
          };
        };
        defaultPackage = packages.wire-cli;
        devShell = packages.dev-shell;
      });
}
