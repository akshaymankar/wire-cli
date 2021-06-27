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
  haskellPackages = super.haskellPackages.override({
    overrides = hself: hsuper: {
      # HLS segfaults for GTK things without this.
      haskell-language-server = self.haskell.lib.appendConfigureFlag hsuper.haskell-language-server "--enable-executable-dynamic";
    };
  });
}
