{ mkDerivation, aeson, base, conduit, containers, cql, cql-io
, cql-io-tinylog, exceptions, fetchgit, hpack, imports, lens
, lens-aeson, lib, optparse-applicative, retry, split, text, time
, tinylog, uuid, wreq
}:
mkDerivation {
  pname = "cassandra-util";
  version = "0.16.5";
  src = fetchgit {
    url = "https://github.com/wireapp/wire-server";
    sha256 = "0pq8kwdzs87ivjci8js8bspz50v15mjr36san88d044wq1jkv4fg";
    rev = "7fe73c7338b397140f1aca12c79f320ac277d8c3";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libs/cassandra-util; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base conduit containers cql cql-io cql-io-tinylog exceptions
    imports lens lens-aeson optparse-applicative retry split text time
    tinylog uuid wreq
  ];
  libraryToolDepends = [ hpack ];
  doHaddock = false;
  doCheck = false;
  prePatch = "hpack";
  description = "Cassandra Utilities";
  license = lib.licenses.agpl3Only;
}
