{ mkDerivation, ascii-case, ascii-caseless, ascii-char, ascii-group
, ascii-numbers, ascii-predicates, ascii-superset, ascii-th, base
, bytestring, hspec, lib, text
}:
mkDerivation {
  pname = "ascii";
  version = "1.5.1.0";
  sha256 = "34121c181be2741b8561891826d66df01fef8cb9f28cd1c95f77013367ee4205";
  libraryHaskellDepends = [
    ascii-case ascii-caseless ascii-char ascii-group ascii-numbers
    ascii-predicates ascii-superset ascii-th base bytestring text
  ];
  testHaskellDepends = [
    ascii-case ascii-caseless ascii-char ascii-group ascii-numbers
    ascii-predicates ascii-superset ascii-th base bytestring hspec text
  ];
  homepage = "https://github.com/typeclasses/ascii";
  description = "The ASCII character set and encoding";
  license = lib.licenses.asl20;
}
