{ mkDerivation, base, bytestring, containers, fetchgit, hpack
, hspec, hspec-discover, http-types, imports, lib, metrics-core
, servant, servant-multipart, string-conversions, text, wai
, wai-middleware-prometheus, wai-route, wai-routing
}:
mkDerivation {
  pname = "metrics-wai";
  version = "0.5.7";
  src = fetchgit {
    url = "https://github.com/wireapp/wire-server";
    sha256 = "0pq8kwdzs87ivjci8js8bspz50v15mjr36san88d044wq1jkv4fg";
    rev = "7fe73c7338b397140f1aca12c79f320ac277d8c3";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libs/metrics-wai; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers http-types imports metrics-core servant
    servant-multipart string-conversions text wai
    wai-middleware-prometheus wai-route wai-routing
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    base bytestring containers hspec http-types imports metrics-core
    servant servant-multipart string-conversions text wai
    wai-middleware-prometheus wai-route wai-routing
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  doCheck = false;
  prePatch = "hpack";
  description = "Metrics WAI integration";
  license = lib.licenses.agpl3Only;
}
