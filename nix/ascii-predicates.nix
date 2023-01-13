{ mkDerivation, ascii-char, base, hedgehog, lib }:
mkDerivation {
  pname = "ascii-predicates";
  version = "1.0.1.2";
  sha256 = "d2cf73dd021ce0fe01d72fc36027ac331ba91370428c03f5cd3f9b15e349932b";
  libraryHaskellDepends = [ ascii-char base ];
  testHaskellDepends = [ ascii-char base hedgehog ];
  homepage = "https://github.com/typeclasses/ascii-predicates";
  description = "Various categorizations of ASCII characters";
  license = lib.licenses.asl20;
}
