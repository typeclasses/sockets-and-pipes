{ mkDerivation, ascii-case, ascii-caseless, ascii-char
, ascii-superset, base, bytestring, hspec, lib, template-haskell
, text
}:
mkDerivation {
  pname = "ascii-th";
  version = "1.2.0.0";
  sha256 = "6f41d2216e473011cc660d05429c1ce2a7c76bdb87cd303a816871974b3a661f";
  libraryHaskellDepends = [
    ascii-case ascii-caseless ascii-char ascii-superset base
    template-haskell
  ];
  testHaskellDepends = [
    ascii-case ascii-caseless ascii-char ascii-superset base bytestring
    hspec text
  ];
  homepage = "https://github.com/typeclasses/ascii-th";
  description = "Template Haskell support for ASCII";
  license = lib.licenses.asl20;
}
