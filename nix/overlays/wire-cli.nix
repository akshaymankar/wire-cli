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
        cargoSha256 = "0zs8ibv7rinrrzp9naxd7yak7kn1gp3pjb3g8i4wf7xw2hkkq81z";
        postInstall = ''
          mkdir -p $out/include
          cp src/cbox.h $out/include
        '';
      }
  ) {};
}
