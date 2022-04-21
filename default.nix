{ mkDerivation, aeson, async, base, base64, bytestring
, bytestring-conversion, containers, cookie, cryptobox-haskell
, deriving-aeson, directory, extra, filepath, generic-random
, ghc-source-gen, gi-gio, gi-gio-hs-list-model, gi-glib, gi-gtk
, gi-pango, haskell-gi-base, HsOpenSSL, HsOpenSSL-x509-system
, hspec, hspec-core, hspec-discover, http-client
, http-client-openssl, http-client-websockets, http-types, keys
, lens-family, lib, network-arbitrary, network-uri
, optparse-applicative, polysemy, polysemy-mocks, polysemy-plugin
, polysemy-zoo, profunctors, proto-lens, proto-lens-arbitrary
, QuickCheck, quickcheck-instances, random, retry
, schema-profunctor, servant, servant-client, servant-client-core
, shelly, swagger2, temporary, text, time, types-common, unagi-chan
, unordered-containers, uuid, websockets, wire-api, wire-message
}:
mkDerivation {
  pname = "wire-cli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base base64 bytestring bytestring-conversion containers
    cookie cryptobox-haskell deriving-aeson directory extra filepath
    ghc-source-gen gi-gio gi-gio-hs-list-model gi-glib gi-gtk gi-pango
    haskell-gi-base HsOpenSSL HsOpenSSL-x509-system http-client
    http-client-openssl http-client-websockets http-types keys
    lens-family network-uri optparse-applicative polysemy
    polysemy-plugin polysemy-zoo profunctors proto-lens
    schema-profunctor servant servant-client servant-client-core
    swagger2 text time types-common unagi-chan unordered-containers
    uuid websockets wire-api wire-message
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base base64 bytestring containers cookie cryptobox-haskell
    generic-random HsOpenSSL HsOpenSSL-x509-system hspec hspec-core
    http-client http-client-openssl http-types lens-family
    network-arbitrary network-uri optparse-applicative polysemy
    polysemy-mocks polysemy-plugin polysemy-zoo proto-lens
    proto-lens-arbitrary QuickCheck quickcheck-instances random retry
    shelly temporary text time types-common unagi-chan uuid wire-api
    wire-message
  ];
  testToolDepends = [ hspec-discover ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
