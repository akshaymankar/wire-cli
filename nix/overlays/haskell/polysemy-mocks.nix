{ mkDerivation, base, hspec, hspec-discover, lib, polysemy
, template-haskell
}:
mkDerivation {
  pname = "polysemy-mocks";
  version = "0.3.0.0";
  sha256 = "ffc0b56e827f1c4d5968a995dccb00326513d0426edc0dfdf4308b97fd2dd80f";
  libraryHaskellDepends = [ base polysemy template-haskell ];
  testHaskellDepends = [ base hspec polysemy ];
  testToolDepends = [ hspec-discover ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/akshaymankar/polysemy-mocks#readme";
  description = "Mocking framework for polysemy effects";
  license = lib.licenses.bsd3;
}
