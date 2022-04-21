{ mkDerivation, base, extra, fetchgit, hpack, imports, lib
, swagger2
}:
mkDerivation {
  pname = "deriving-swagger2";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/wireapp/wire-server";
    sha256 = "0pq8kwdzs87ivjci8js8bspz50v15mjr36san88d044wq1jkv4fg";
    rev = "7fe73c7338b397140f1aca12c79f320ac277d8c3";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libs/deriving-swagger2; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base extra imports swagger2 ];
  libraryToolDepends = [ hpack ];
  doHaddock = false;
  doCheck = false;
  prePatch = "hpack";
  license = lib.licenses.agpl3Only;
}
