{ mkDerivation, attoparsec, base, bytestring, lib, text }:
mkDerivation {
  pname = "attoparsec-run";
  version = "0.0.1.0";
  sha256 = "2520c5dbe7b151486b62770fd5c477d3ca1f088720aa4d8fec80f54b8451b169";
  libraryHaskellDepends = [ attoparsec base bytestring text ];
  homepage = "https://github.com/typeclasses/attoparsec-run";
  description = "Conveniently run Attoparsec parsers";
  license = lib.licenses.asl20;
}
