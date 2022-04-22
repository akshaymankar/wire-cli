hsuper: hself: {
  cryptobox-haskell = hself.callPackage ./cryptobox-haskell.nix {};
  wire-api = hself.callPackage ./wire-api.nix {};
  schema-profunctor = hself.callPackage ./schema-profunctor.nix {};
  wire-message-proto-lens = hself.callPackage ./wire-message-proto-lens.nix {};
  imports = hself.callPackage ./imports.nix {};
  types-common = hself.callPackage ./types-common.nix {};
  cassandra-util = hself.callPackage ./cassandra-util.nix {};
  extended = hself.callPackage ./extended.nix {};
  deriving-swagger2 = hself.callPackage ./deriving-swagger2.nix {};
  hscim = hself.callPackage ./hscim.nix {};
  metrics-wai = hself.callPackage ./metrics-wai.nix {};
  metrics-core = hself.callPackage ./metrics-core.nix {};
  wai-utilities = hself.callPackage ./wai-utilities.nix {};
  saml2-web-sso = hself.callPackage ./saml2-web-sso.nix {};
  proto-lens-arbitrary = hself.callPackage ./proto-lens-arbitrary.nix {};
  hsaml2 = hself.callPackage ./hsaml2.nix {};
  swagger = hself.callPackage ./swagger.nix {};
  wai-routing = hself.callPackage ./wai-routing.nix {};
  polysemy-mocks = hself.callPackage ./polysemy-mocks.nix {};
  partial-isomorphisms = hself.callPackage ./partial-isomorphisms.nix {};
  singletons = hself.callPackage ./singletons.nix {};
  hspec-wai = hself.callPackage ./hspec-wai.nix {};
  th-desugar = hself.callPackage ./th-desugar.nix {};
  wai-route = hself.callPackage ./wai-route.nix {};
}
