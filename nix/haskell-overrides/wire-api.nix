{ mkDerivation, aeson, aeson-pretty, aeson-qq, attoparsec, base
, base64-bytestring, binary, binary-parsers, bytestring
, bytestring-arbitrary, bytestring-conversion, case-insensitive
, cassandra-util, cassava, cereal, comonad, conduit, constraints
, containers, cookie, cryptonite, currency-codes, deriving-aeson
, deriving-swagger2, directory, either, email-validate, errors
, extended, extra, fetchgit, filepath, generic-random, generics-sop
, ghc-prim, hashable, hex, hostname-validate, hpack, hscim
, http-api-data, http-media, http-types, imports
, insert-ordered-containers, iproute, iso3166-country-codes, iso639
, lens, lib, memory, metrics-wai, mime, mtl, pem, polysemy, pretty
, proto-lens, protobuf, QuickCheck, quickcheck-instances, random
, resourcet, saml2-web-sso, schema-profunctor, servant
, servant-client, servant-client-core, servant-conduit
, servant-multipart, servant-server, servant-swagger
, servant-swagger-ui, singletons, sop-core, string-conversions
, swagger, swagger2, tagged, tasty, tasty-expected-failure
, tasty-hunit, tasty-quickcheck, text, time, types-common
, unordered-containers, uri-bytestring, uuid, vector, wai
, wai-extra, wai-utilities, wai-websockets, websockets
, wire-message-proto-lens, x509
}:
mkDerivation {
  pname = "wire-api";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/wireapp/wire-server";
    sha256 = "0pq8kwdzs87ivjci8js8bspz50v15mjr36san88d044wq1jkv4fg";
    rev = "7fe73c7338b397140f1aca12c79f320ac277d8c3";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/libs/wire-api; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring binary binary-parsers
    bytestring bytestring-conversion case-insensitive cassandra-util
    cassava cereal comonad conduit constraints containers cookie
    cryptonite currency-codes deriving-aeson deriving-swagger2 either
    email-validate errors extended extra filepath generic-random
    generics-sop ghc-prim hashable hostname-validate hscim
    http-api-data http-media http-types imports
    insert-ordered-containers iproute iso3166-country-codes iso639 lens
    memory metrics-wai mime mtl pem polysemy proto-lens protobuf
    QuickCheck quickcheck-instances random resourcet saml2-web-sso
    schema-profunctor servant servant-client servant-client-core
    servant-conduit servant-multipart servant-server servant-swagger
    servant-swagger-ui singletons sop-core string-conversions swagger
    swagger2 tagged text time types-common unordered-containers
    uri-bytestring uuid vector wai wai-extra wai-utilities
    wai-websockets websockets wire-message-proto-lens x509
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson aeson-pretty aeson-qq base bytestring bytestring-arbitrary
    bytestring-conversion case-insensitive cassava containers
    currency-codes directory either filepath hex hscim imports
    iso3166-country-codes iso639 lens metrics-wai mime pem pretty
    proto-lens QuickCheck saml2-web-sso schema-profunctor servant
    servant-swagger-ui string-conversions swagger2 tasty
    tasty-expected-failure tasty-hunit tasty-quickcheck text time
    types-common unordered-containers uri-bytestring uuid vector
    wire-message-proto-lens
  ];
  doHaddock = false;
  doCheck = false;
  prePatch = "hpack";
  license = lib.licenses.agpl3Only;
}
