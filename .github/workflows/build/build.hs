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
                      "ascii"      == "1.0.0.0"   <>
                      "async"      == "2.2.1"     <>
                      "base"       == "4.12.0.0"  <>
                      "blaze-html" == "0.9.0.0"   <>
                      "bytestring" == "0.10.8.0"  <>
                      "containers" == "0.5.7.0"   <>
                      "network"    == "3.1.2.0"   <>
                      "pipes"      == "4.3.9"     <>
                      "time"       == "1.9"

    GHC 8  8  _   ->  "aeson"      ^>= "1.4"      <>
                      "base"       ^>= "4.13"

    GHC 8 10  _   ->  "base"       ^>= "4.14"     <>
                      "time"       ^>= "1.10"

    GHC 9 0   _   ->  "aeson"      ^>= "1.5.6.0"  <>
                      "ascii"      ^>= "1.0.1.4"  <>
                      "async"      ^>= "2.2.3"    <>
                      "base"       ^>= "4.15"     <>
                      "blaze-html" ^>= "0.9.1.2"  <>
                      "bytestring" ^>= "0.11.1.0" <>
                      "containers" ^>= "0.6.4.1"  <>
                      "network"    ^>= "3.1.2.1"  <>
                      "pipes"      ^>= "4.3.15"   <>
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
