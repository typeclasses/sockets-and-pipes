import Relude hiding ((==))
import System.Environment
import System.Process
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

main = readEnv >>= test

readEnv = getEnv "ghc" >>= \t ->
    let Just x = parseMaybe ghcP t in return x

ghcP :: Parser GHC
ghcP = GHC <$> (decimal <* char '.')
           <*> (decimal <* char '.')
           <*> decimal

test = testConstraints . figureConstraints

figureConstraints =
  \case
    GHC 8  6  _   ->  "aeson"      == "1.3.0.0"   <>
                      "base"       == "4.12.0.0"  <>
                      "containers" == "0.5.7.0"   <>
                      "network"    == "3.1.2.0"   <>
                      "time"       == "1.9"

    GHC 8  8  _   ->  "aeson"      ^>= "1.4"   <>
                      "base"       ^>= "4.13"

    GHC 8 10  _   ->  "base"       ^>= "4.14"  <>
                      "time"       ^>= "1.10"

    GHC 9 0   _   ->  "aeson"      ^>= "1.5"   <>
                      "base"       ^>= "4.15"  <>
                      "containers" ^>= "0.6"   <>
                      "time"       ^>= "1.11"

a ^>= b = [ ConstraintCaret (Package a) (Version b) ]

a == b = [ ConstraintExact (Package a) (Version b) ]

testConstraints cs = callProcess "cabal" $
    "build" : map (toString . constraintFlag) cs

constraintFlag (ConstraintCaret (Package a) (Version b)) =
    "--constraint=" <> a <> " ^>= " <> b
constraintFlag (ConstraintExact (Package a) (Version b)) =
    "--constraint=" <> a <> " == " <> b


---  Types  ---

type Parser = Parsec Void String

data GHC = GHC Integer Integer Integer
  -- ^ A GHC version number, like GHC 8.10.3

newtype Package = Package Text
  -- ^ A package name, like "containers"

newtype Version = Version Text
  -- ^ A version number, like "0.6"

data Constraint =
    ConstraintCaret Package Version
      -- ^ A package constraint like "containers ^>= 0.6"
  | ConstraintExact Package Version
      -- ^ A package constraint like "containers == 0.6"
