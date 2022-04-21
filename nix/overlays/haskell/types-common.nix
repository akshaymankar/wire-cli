{ mkDerivation, aeson, attoparsec, attoparsec-iso8601, base
, base16-bytestring, base64-bytestring, binary, bytestring
, bytestring-conversion, cassandra-util, cereal, containers
, cryptohash-md5, cryptohash-sha1, cryptonite, data-default
, fetchgit, hashable, hpack, http-api-data, imports, iproute, lens
, lens-datetime, lib, optparse-applicative, protobuf, QuickCheck
, quickcheck-instances, random, schema-profunctor, scientific
, servant-server, singletons, string-conversions, swagger, swagger2
, tagged, tasty, tasty-hunit, tasty-quickcheck, text, time
, time-locale-compat, tinylog, unix, unordered-containers
, uri-bytestring, uuid, vector, yaml
}:
mkDerivation {
  pname = "types-common";
  version = "0.16.0";
  src = fetchgit {
    url = "https://github.com/wireapp/wire-server";
    sha256 = "0pq8kwdzs87ivjci8js8bspz50v15mjr36san88d044wq1jkv4fg";
    rev = "7fe73c7338b397140f1aca12c79f320ac277d8c3";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libs/types-common; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec attoparsec-iso8601 base base16-bytestring
    base64-bytestring binary bytestring bytestring-conversion
    cassandra-util containers cryptohash-md5 cryptohash-sha1 cryptonite
    data-default hashable http-api-data imports iproute lens
    lens-datetime optparse-applicative protobuf QuickCheck
    quickcheck-instances random schema-profunctor scientific
    servant-server singletons string-conversions swagger swagger2
    tagged tasty text time time-locale-compat tinylog unix
    unordered-containers uri-bytestring uuid vector yaml
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson base base16-bytestring base64-bytestring bytestring
    bytestring-conversion cereal imports protobuf QuickCheck
    string-conversions tasty tasty-hunit tasty-quickcheck text time
    unordered-containers uuid
  ];
  doHaddock = false;
  doCheck = false;
  prePatch = "hpack";
  description = "Shared type definitions";
  license = lib.licenses.agpl3Only;
}
