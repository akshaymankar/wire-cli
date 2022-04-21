{ mkDerivation, aeson, async, base, bytestring
, bytestring-conversion, errors, exceptions, fetchgit, hpack
, http-types, imports, kan-extensions, lib, metrics-core
, metrics-wai, pipes, prometheus-client, streaming-commons
, string-conversions, swagger, text, tinylog, types-common, unix
, wai, wai-predicates, wai-routing, warp, warp-tls
}:
mkDerivation {
  pname = "wai-utilities";
  version = "0.16.1";
  src = fetchgit {
    url = "https://github.com/wireapp/wire-server";
    sha256 = "0pq8kwdzs87ivjci8js8bspz50v15mjr36san88d044wq1jkv4fg";
    rev = "7fe73c7338b397140f1aca12c79f320ac277d8c3";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libs/wai-utilities; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson async base bytestring bytestring-conversion errors exceptions
    http-types imports kan-extensions metrics-core metrics-wai pipes
    prometheus-client streaming-commons string-conversions swagger text
    tinylog types-common unix wai wai-predicates wai-routing warp
    warp-tls
  ];
  libraryToolDepends = [ hpack ];
  doHaddock = false;
  doCheck = false;
  prePatch = "hpack";
  description = "Various helpers for WAI";
  license = lib.licenses.agpl3Only;
}
