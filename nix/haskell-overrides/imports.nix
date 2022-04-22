{ mkDerivation, base, bytestring, containers, deepseq, extra
, fetchgit, hpack, lib, mtl, text, transformers, unliftio
, unliftio-core, unordered-containers
}:
mkDerivation {
  pname = "imports";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/wireapp/wire-server";
    sha256 = "0pq8kwdzs87ivjci8js8bspz50v15mjr36san88d044wq1jkv4fg";
    rev = "7fe73c7338b397140f1aca12c79f320ac277d8c3";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libs/imports; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers deepseq extra mtl text transformers
    unliftio unliftio-core unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  doHaddock = false;
  doCheck = false;
  prePatch = "hpack";
  description = "Very common imports";
  license = lib.licenses.agpl3Only;
}
