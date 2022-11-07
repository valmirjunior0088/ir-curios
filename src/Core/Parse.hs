module Core.Parse
  ( parse
  )
  where

import Core.Syntax
  ( Variable (..)
  , Scope
  , unbound
  , PrimitiveType (..)
  , Primitive (..)
  , Operation (..)
  , Term (..)
  , Walk
  , abstract
  )

import Text.Megaparsec
  ( ParsecT
  , runParserT
  , some
  , someTill
  , manyTill
  , try
  , oneOf
  , (<|>)
  , eof
  , single
  , optional
  )

import Text.Megaparsec.Char.Lexer
  ( space
  , skipLineComment
  , skipBlockComment
  , lexeme
  , symbol
  , decimal
  , float
  )

import Data.Void (Void)
import Data.Functor ((<&>))
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import Text.Megaparsec.Char (space1)
import Control.Monad.Reader (MonadReader (..), Reader, runReader, asks)
import Core.Program (Entry (..), Program (..))

type Parse = ParsecT Void String (Reader [String])

runParse :: Parse a -> String -> Either (ParseErrorBundle String Void) a
runParse action source = runReader (runParserT action "" source) []

parseSpace :: Parse ()
parseSpace = space space1 (skipLineComment "//") (skipBlockComment "/*" "*/")

parseLexeme :: Parse a -> Parse a
parseLexeme = lexeme parseSpace

parseSymbol :: String -> Parse String
parseSymbol = symbol parseSpace

parseIdentifier :: Parse String
parseIdentifier = parseLexeme (some $ try $ oneOf validCharacters) where
  validCharacters = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_']

parseSplit :: Parse Term
parseSplit = do
  scrutinee <- parseSymbol "split " *> parseTerm "{"
  left <- parseSymbol "|" *> parseIdentifier
  right <- parseSymbol "," *> parseIdentifier <* parseSymbol "|"
  body <- parseScope left (parseScope right (parseTerm "}"))
  return (Split scrutinee body)

parseLabelType :: Parse Term
parseLabelType = do
  labels <- parseSymbol "{" *> manyTill parseIdentifier (parseSymbol "}")
  return (LabelType labels)

parseLabel :: Parse Term
parseLabel = do
  label <- single ':' *> parseIdentifier
  return (Label label)

parseBranch :: Parse (String, Term)
parseBranch = do
  label <- parseSymbol "|" *> parseIdentifier <* parseSymbol "|"
  body <- parseTerm ";"
  return (label, body)

parseMatch :: Parse Term
parseMatch = do
  scrutinee <- parseSymbol "match " *> parseTerm "{"
  branches <- manyTill parseBranch (parseSymbol "}")
  return (Match scrutinee branches)

parseParens :: Parse Term
parseParens = parseSymbol "(" *> parseTerm ")"

parseScope :: Walk a => String -> Parse a -> Parse (Scope a)
parseScope identifier parser = do
  scope <- local (identifier :) parser
  return (abstract identifier scope)

parseUnboundScope :: Parse a -> Parse (Scope a)
parseUnboundScope parser = unbound <$> parser

parseName :: Parse Term
parseName = parseIdentifier >>= \case
  "Type" ->
    return Type

  "Int32" ->
    return (PrimitiveType Int32Type)

  "Flt32" ->
    return (PrimitiveType Flt32Type)

  identifier -> asks (elem identifier) <&> \case
    True -> Local (Free identifier)
    False -> Global identifier

parseInt :: Parse Primitive
parseInt = positive <|> negative where
  positive = Int32 <$> (optional (single '+') *> decimal)
  negative = Int32 <$> (single '-' *> (negate <$> decimal))

parseFloat :: Parse Primitive
parseFloat = positive <|> negative where
  positive = Flt32 <$> (optional (single '+') *> float)
  negative = Flt32 <$> (single '-' *> (negate <$> float))

parsePrimitive :: Parse Term
parsePrimitive = Primitive <$> parseLexeme (parseInt <|> parseFloat)

parseOperation :: Parse Operation
parseOperation =
  Int32Add <$ parseSymbol "int32.add" <|> Flt32Add <$ parseSymbol "flt32.add"

parseOperate :: Parse Term
parseOperate = do
  operation <- try (parseSymbol "[" *> parseOperation)
  parameters <- someTill parseClosed (parseSymbol "]")
  return (Operate operation parameters)

parseClosed :: Parse Term
parseClosed = try parseSplit
  <|> try parseLabelType
  <|> try parseLabel
  <|> try parseMatch
  <|> try parsePrimitive
  <|> try parseOperate
  <|> try parseParens
  <|> parseName

parseApply :: String -> Parse Term
parseApply boundary = do
  terms <- someTill parseClosed (parseSymbol boundary)
  return (foldl1 Apply terms)

parsePairType :: String -> Parse Term
parsePairType boundary = try parser <|> parseApply boundary where
  parseDependent = do
    identifier <- parseSymbol "(" *> parseIdentifier
    input <- parseSymbol ":" *> parseTerm ")" <* parseSymbol "*>"
    scope <- parseScope identifier (parsePairType boundary)
    return (PairType input scope)

  parseNonDependent = do
    input <- parseApply "*>"
    scope <- parseUnboundScope (parsePairType boundary)
    return (PairType input scope)

  parser = try parseDependent <|> parseNonDependent

parsePair :: String -> Parse Term
parsePair boundary = try parser <|> parseApply boundary where
  parser = do
    left <- parseApply ","
    right <- parsePair boundary
    return (Pair left right)

parseFunctionType :: String -> Parse Term
parseFunctionType boundary = try parser <|> parseApply boundary where
  parseDependent = do
    identifier <- parseSymbol "(" *> parseIdentifier
    input <- parseSymbol ":" *> parseTerm ")" <* parseSymbol "->"
    scope <- parseScope identifier (parseFunctionType boundary)
    return (FunctionType input scope)

  parseNonDependent = do
    input <- parseApply "->"
    scope <- parseUnboundScope (parseFunctionType boundary)
    return (FunctionType input scope)

  parser = try parseDependent <|> parseNonDependent

parseFunction :: String -> Parse Term
parseFunction boundary = try parser <|> fallback where
  parser = do
    identifier <- parseIdentifier <* parseSymbol "=>"
    body <- parseScope identifier (parseFunction boundary)
    return (Function body)

  fallback = try (parseFunctionType boundary)
    <|> try (parsePair boundary)
    <|> parsePairType boundary

parseTerm :: String -> Parse Term
parseTerm = parseFunction

parseEntry :: Parse Entry
parseEntry = try parseDeclaration <|> parseDefinition where
  parseDeclaration = do
    identifier <- parseIdentifier <* parseSymbol ":"
    declaration <- parseTerm ";"
    return (Declaration identifier declaration)

  parseDefinition = do
    identifier <- parseIdentifier <* parseSymbol "="
    definition <- parseTerm ";"
    return (Definition identifier definition)

parseProgram :: Parse Program
parseProgram = Program <$> someTill parseEntry eof

parse :: String -> Either String Program
parse source = case runParse parseProgram source of
  Left bundle -> Left (errorBundlePretty bundle)
  Right program -> Right program
