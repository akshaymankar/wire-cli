{ mkDerivation, base, Cabal, fetchgit, lib, proto-lens-protoc
, proto-lens-runtime, proto-lens-setup
}:
mkDerivation {
  pname = "wire-message-proto-lens";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/wireapp/wire-server";
    sha256 = "0pq8kwdzs87ivjci8js8bspz50v15mjr36san88d044wq1jkv4fg";
    rev = "7fe73c7338b397140f1aca12c79f320ac277d8c3";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libs/wire-message-proto-lens; echo source root reset to $sourceRoot";
  setupHaskellDepends = [ base Cabal proto-lens-setup ];
  libraryHaskellDepends = [ base proto-lens-runtime ];
  libraryToolDepends = [ proto-lens-protoc ];
  doHaddock = false;
  doCheck = false;
  description = "Shared protobuf type definitions for Wire Messaging";
  license = lib.licenses.agpl3Only;
}
