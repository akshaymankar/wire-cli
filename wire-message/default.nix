{ mkDerivation, base, Cabal, lib, proto-lens-protoc
, proto-lens-runtime, proto-lens-setup
}:
mkDerivation {
  pname = "wire-message";
  version = "0.1.0.0";
  src = ./.;
  setupHaskellDepends = [ base Cabal proto-lens-setup ];
  libraryHaskellDepends = [ base proto-lens-runtime ];
  libraryToolDepends = [ proto-lens-protoc ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
