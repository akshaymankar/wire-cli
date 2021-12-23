self: super: {
  # TODO: Do not use buildRustPackage. Ces't horrible
  cryptobox = self.callPackage (
    { fetchFromGitHub, rustPlatform, pkgconfig, libsodium }:
      rustPlatform.buildRustPackage rec {
        name = "cryptobox-c-${version}";
        version = "2019-06-17";
        nativeBuildInputs = [ pkgconfig ];
        buildInputs = [ libsodium ];
        src = fetchFromGitHub {
          owner = "wireapp";
          repo = "cryptobox-c";
          rev = "4067ad96b125942545dbdec8c1a89f1e1b65d013";
          sha256 = "1i9dlhw0xk1viglyhail9fb36v1awrypps8jmhrkz8k1bhx98ci3";
        };
        cargoSha256 = "123jza3awrixgm35la3y3fqsixp7q2b0jpnp809jq34p215ggyh1";
        postInstall = ''
          mkdir -p $out/include
          cp src/cbox.h $out/include
        '';
      }
  ) {};
  haskellPackages = super.haskellPackages.override(
    let headCabalRepo =  super.fetchFromGitHub {
          owner = "haskell";
          repo = "cabal";
          rev = "69563140fc2f5e68d31bbd3102a12b3c14fc9bda";
          sha256 = "sha256-so0vKJ2JtLehTrCRh55WsGsuBiplHGFblJfwmPRoupg=";
        };
    in {
      overrides = hself: hsuper: rec {
        # HLS segfaults for GTK things without this.
        haskell-language-server = self.haskell.lib.appendConfigureFlag hsuper.haskell-language-server "--enable-executable-dynamic";

        # Use head cabal for supporting submodules in git dependecies:
        # https://github.com/haskell/cabal/pull/7625
        Cabal-head = hsuper.Cabal_3_6_2_0.overrideAttrs (oldAttrs: {
          version = "3.7.0.0";
          src = headCabalRepo + "/Cabal";
        });
        hackage-security-head = hsuper.hackage-security.override({
          Cabal = Cabal-head;
          # base16-bytestring = hsuper.base16-bytestring_0_1_1_7;
        });
        cabal-install-solver-head =
          let hackedDrv = hsuper.Cabal_3_6_2_0.overrideAttrs (oldAttrs: {
                version = "3.7.0.0";
                pname = "cabal-install-solver";
                src = headCabalRepo + "/cabal-install-solver";
              });
              withDeps = self.haskell.lib.addExtraLibraries hackedDrv [
                Cabal-head
                hsuper.edit-distance
              ];
          in withDeps;
        cabal-install-head =
          let withHeadDeps = hsuper.cabal-install.override({
                Cabal = Cabal-head;
                hackage-security = hackage-security-head;
              });
              withNewSource = withHeadDeps.overrideAttrs(oldAttrs: {
                version = "3.7.0.0";
                src = headCabalRepo + "/cabal-install";
              });
              withNewDeps = self.haskell.lib.addExtraLibrary withNewSource cabal-install-solver-head;
          in withNewDeps;
    };
  });
}
