{ mkDerivation, aeson, aeson-qq, base, bifunctors, comonad
, containers, fetchgit, hpack, imports, insert-ordered-containers
, lens, lib, profunctors, swagger2, tasty, tasty-hunit, text
, transformers, vector
}:
mkDerivation {
  pname = "schema-profunctor";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/wireapp/wire-server";
    sha256 = "0pq8kwdzs87ivjci8js8bspz50v15mjr36san88d044wq1jkv4fg";
    rev = "7fe73c7338b397140f1aca12c79f320ac277d8c3";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libs/schema-profunctor; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base bifunctors comonad containers imports lens profunctors
    swagger2 text transformers vector
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson aeson-qq base imports insert-ordered-containers lens swagger2
    tasty tasty-hunit text
  ];
  doHaddock = false;
  doCheck = false;
  prePatch = "hpack";
  license = lib.licenses.agpl3Only;
}
