{ mkDerivation, aeson, base, bytestring, cassandra-util, containers
, errors, exceptions, extra, fetchgit, hpack, hspec, hspec-discover
, http-types, imports, lib, metrics-wai, optparse-applicative
, servant, servant-server, servant-swagger, string-conversions
, temporary, tinylog, wai
}:
mkDerivation {
  pname = "extended";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/wireapp/wire-server";
    sha256 = "0pq8kwdzs87ivjci8js8bspz50v15mjr36san88d044wq1jkv4fg";
    rev = "7fe73c7338b397140f1aca12c79f320ac277d8c3";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libs/extended; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base bytestring cassandra-util containers errors exceptions
    extra http-types imports metrics-wai optparse-applicative servant
    servant-server servant-swagger string-conversions tinylog wai
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base bytestring cassandra-util containers errors exceptions
    extra hspec http-types imports metrics-wai optparse-applicative
    servant servant-server servant-swagger string-conversions temporary
    tinylog wai
  ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  doCheck = false;
  prePatch = "hpack";
  description = "Extended versions of common modules";
  license = lib.licenses.agpl3Only;
}
