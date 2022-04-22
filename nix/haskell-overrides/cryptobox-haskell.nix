{ mkDerivation, base, bytestring, cryptobox, fetchgit, hashable
, lib, unordered-containers
}:
mkDerivation {
  pname = "cryptobox-haskell";
  version = "0.1.1";
  src = fetchgit {
    url = "https://github.com/wireapp/cryptobox-haskell";
    sha256 = "1mf9wv5w4vmp36srabaq6nwmimvyjkvfiwm4krbdnf22vax7n2sn";
    rev = "3a65e2fa055ae2f4ffaea4c168a3edfe8958d84d";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base bytestring hashable unordered-containers
  ];
  librarySystemDepends = [ cryptobox ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/wireapp/cryptobox-haskell/";
  description = "Haskell bindings to cryptobox";
  license = lib.licenses.gpl3Only;
}
