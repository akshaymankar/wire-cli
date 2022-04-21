{ mkDerivation, base, containers, fetchgit, hashable, hpack
, immortal, imports, lib, prometheus-client, text
, unordered-containers
}:
mkDerivation {
  pname = "metrics-core";
  version = "0.3.2";
  src = fetchgit {
    url = "https://github.com/wireapp/wire-server";
    sha256 = "0pq8kwdzs87ivjci8js8bspz50v15mjr36san88d044wq1jkv4fg";
    rev = "7fe73c7338b397140f1aca12c79f320ac277d8c3";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libs/metrics-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base containers hashable immortal imports prometheus-client text
    unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  doHaddock = false;
  doCheck = false;
  prePatch = "hpack";
  description = "Metrics core";
  license = lib.licenses.agpl3Only;
}
