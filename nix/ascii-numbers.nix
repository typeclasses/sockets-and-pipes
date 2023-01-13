{ mkDerivation, ascii-case, ascii-char, ascii-superset, base
, bytestring, hashable, hedgehog, invert, lib, text
}:
mkDerivation {
  pname = "ascii-numbers";
  version = "1.1.0.2";
  sha256 = "dbfe4acb0eaa2f0c4ce68c6ac6072297cc485b5c7f9b3ad59375c36133b61837";
  revision = "2";
  editedCabalFile = "19x9mh11pb7j4ykf9vicprn6mlhcb9gwsk82gh5yk366k4r172d7";
  libraryHaskellDepends = [
    ascii-case ascii-char ascii-superset base bytestring hashable text
  ];
  testHaskellDepends = [
    ascii-case ascii-char ascii-superset base bytestring hashable
    hedgehog invert text
  ];
  homepage = "https://github.com/typeclasses/ascii-numbers";
  description = "ASCII representations of numbers";
  license = lib.licenses.asl20;
}
