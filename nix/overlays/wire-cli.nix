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
          rev = "d720c94718a677af2e27ff7951a1198bc95817a9";
          sha256 = "sha256-4ia8P3ghl3qYYz8tRVv47BJAcL8dMAgNL8nE+MMKz9g=";
        };
    in {
      overrides = hself: hsuper: rec {
        # HLS segfaults for GTK things without this.
        haskell-language-server = self.haskell.lib.appendConfigureFlag hsuper.haskell-language-server "--enable-executable-dynamic";

        Cabal-syntax = hsuper.callPackage ./Cabal-syntax.nix {
          src = headCabalRepo + "/Cabal-syntax";
        };
        # Use head cabal for supporting submodules in git dependecies:
        # https://github.com/haskell/cabal/pull/7625
        Cabal-head =
          let hackedDrv = hsuper.Cabal_3_6_2_0.overrideAttrs (oldAttrs: {
                version = "3.7.0.0";
                src = headCabalRepo + "/Cabal";
                prePatch = "";
              });
              withDeps = self.haskell.lib.addExtraLibraries hackedDrv [
                Cabal-syntax
              ];
          in withDeps;
        hackage-security-head = hsuper.hackage-security.override({
          Cabal = Cabal-head;
          # base16-bytestring = hsuper.base16-bytestring_0_1_1_7;
        });
        cabal-install-solver-head =
          let hackedDrv = hsuper.Cabal_3_6_2_0.overrideAttrs (oldAttrs: {
                version = "3.7.0.0";
                pname = "cabal-install-solver";
                src = headCabalRepo + "/cabal-install-solver";
                prePatch = "";
              });
              withDeps = self.haskell.lib.addExtraLibraries hackedDrv [
                Cabal-head
                hsuper.edit-distance
                Cabal-syntax
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
                prePatch = "";
              });
              withNewDeps = self.haskell.lib.addExtraLibrary withNewSource cabal-install-solver-head;
          in withNewDeps;
    };
  });
}
