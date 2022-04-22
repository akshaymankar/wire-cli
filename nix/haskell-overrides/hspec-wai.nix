{ mkDerivation, base, base-compat, bytestring, case-insensitive
, hspec, hspec-core, hspec-expectations, http-types, lib
, QuickCheck, text, transformers, wai, wai-extra
}:
mkDerivation {
  pname = "hspec-wai";
  version = "0.9.2";
  sha256 = "055e414bd6531d3454496f9c4bfa1164b861aa9a9102867d7ffeef8d3a92283f";
  libraryHaskellDepends = [
    base base-compat bytestring case-insensitive hspec-core
    hspec-expectations http-types QuickCheck text transformers wai
    wai-extra
  ];
  testHaskellDepends = [
    base base-compat bytestring case-insensitive hspec hspec-core
    hspec-expectations http-types QuickCheck text transformers wai
    wai-extra
  ];
  doHaddock = false;
  doCheck = false;
  homepage = "https://github.com/hspec/hspec-wai#readme";
  description = "Experimental Hspec support for testing WAI applications";
  license = lib.licenses.mit;
}
