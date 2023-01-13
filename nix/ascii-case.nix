{ mkDerivation, ascii-char, base, hashable, hspec, lib }:
mkDerivation {
  pname = "ascii-case";
  version = "1.0.1.2";
  sha256 = "cd1e13e7f1a296faf9003515d46f77c65686bd2d3ac305dc8dc43b570cbbf89d";
  libraryHaskellDepends = [ ascii-char base hashable ];
  testHaskellDepends = [ ascii-char base hspec ];
  homepage = "https://github.com/typeclasses/ascii-case";
  description = "ASCII letter case";
  license = lib.licenses.asl20;
}
