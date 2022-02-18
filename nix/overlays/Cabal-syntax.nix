{ mkDerivation, array, base, binary, bytestring, containers
, deepseq, directory, fetchgit, filepath, lib, mtl, parsec, pretty
, text, time, transformers, unix
, src
}:
mkDerivation {
  inherit src;
  pname = "Cabal-syntax";
  version = "3.7.0.0";
  libraryHaskellDepends = [
    array base binary bytestring containers deepseq directory filepath
    mtl parsec pretty text time transformers unix
  ];
  homepage = "http://www.haskell.org/cabal/";
  description = "A library for working with .cabal files";
  license = lib.licenses.bsd3;
}
