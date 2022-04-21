{ mkDerivation, base, bytestring, containers, fetchgit, hpack
, lens-family, lib, proto-lens, QuickCheck, text
}:
mkDerivation {
  pname = "proto-lens-arbitrary";
  version = "0.1.2.9";
  src = fetchgit {
    url = "https://github.com/google/proto-lens";
    sha256 = "0l69s292shypgiv2icqhmiwijqhdhqkh6pzri69chl48gbc9dy0r";
    rev = "95d14dc6c3ce5ff9fe30f87245bbe3879a05c9a9";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/proto-lens-arbitrary; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    base bytestring containers lens-family proto-lens QuickCheck text
  ];
  libraryToolDepends = [ hpack ];
  doHaddock = false;
  doCheck = false;
  prePatch = "hpack";
  homepage = "https://github.com/google/proto-lens#readme";
  description = "Arbitrary instances for proto-lens";
  license = lib.licenses.bsd3;
}
