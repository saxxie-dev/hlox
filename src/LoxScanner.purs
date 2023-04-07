module LoxScanner
  ( LoxToken(..)
  , loxScanner
  , parseComment
  , parseKeyword
  , parseLiteral
  , parseSpecialCharToken
  , parseToken
  )
  where

import Parsing.Combinators
import Parsing.String
import Prelude

import Data.Array as Array
import Data.CodePoint.Unicode (isNumber)
import Data.Generic.Rep (class Generic)
import Data.List (List(Cons))
import Data.List.NonEmpty (toList)
import Data.Maybe (Maybe)
import Data.Number as Number
import Data.Show.Generic (genericShow)
import Data.String (Pattern(..), codePointFromChar, fromCodePointArray, split)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (fst)
import Parsing (Parser, liftMaybe)
import Parsing.String.Basic (digit, noneOf, oneOf)

data LoxToken
  -- Special-character tokens.
  = LEFT_PAREN
  | RIGHT_PAREN
  | LEFT_BRACE
  | RIGHT_BRACE
  | COMMA
  | DOT
  | MINUS
  | PLUS
  | SEMICOLON
  | SLASH
  | STAR
  | BANG
  | BANG_EQUAL
  | EQUAL
  | EQUAL_EQUAL
  | GREATER
  | GREATER_EQUAL
  | LESS
  | LESS_EQUAL
  -- Literals.
  | IDENTIFIER String
  | STRING String
  | NUMBER Number
  -- Keywords.
  | AND
  | CLASS
  | ELSE
  | FALSE
  | FUN
  | FOR
  | IF
  | NIL
  | OR
  | PRINT
  | RETURN
  | SUPER
  | THIS
  | TRUE
  | VAR
  | WHILE

derive instance eqLoxToken :: Eq LoxToken
derive instance genericLoxToken :: Generic LoxToken _
instance showLoxToken :: Show LoxToken where
  show = genericShow

loxScanner :: Parser String (List LoxToken)
loxScanner = do 
  result <- many parseToken
  eof
  pure result

parseToken :: Parser String LoxToken 
parseToken = do
  _ <- many $ parseIgnored
  parseKeyword <|> parseSpecialCharToken <|> parseLiteral

parseIgnored :: Parser String Unit
parseIgnored = choice [
  char ' ' *> pure unit, 
  char '\t' *> pure unit, 
  char '\n' *> pure unit,
  char '\r' *> pure unit,
  empty *> pure unit, parseComment]

parseKeyword :: Parser String LoxToken -- TODO include position info in monadT parameter
parseKeyword 
   =  string "and" *> pure AND
  <|> string "class" *> pure CLASS
  <|> string "else" *> pure ELSE
  <|> string "false" *> pure FALSE
  <|> string "fun" *> pure FUN
  <|> string "for" *> pure FOR
  <|> string "if" *> pure IF
  <|> string "nil" *> pure NIL
  <|> string "or" *> pure OR
  <|> string "print" *> pure PRINT
  <|> string "return" *> pure RETURN
  <|> string "super" *> pure SUPER
  <|> string "this" *> pure THIS
  <|> string "true" *> pure TRUE
  <|> string "var" *> pure VAR
  <|> string "while" *> pure WHILE

parseSpecialCharToken :: Parser String LoxToken 
parseSpecialCharToken
   =  string "(" *> pure LEFT_PAREN
  <|> string ")" *> pure RIGHT_PAREN
  <|> string "{" *> pure LEFT_BRACE
  <|> string "}" *> pure RIGHT_BRACE
  <|> string "," *> pure COMMA
  <|> string "." *> pure DOT
  <|> string "-" *> pure MINUS
  <|> string "+" *> pure PLUS
  <|> string ";" *> pure SEMICOLON
  <|> string "/" *> pure SLASH
  <|> string "*" *> pure STAR
  <|> string "!=" *> pure BANG_EQUAL
  <|> string "!" *> pure BANG
  <|> string "==" *> pure EQUAL_EQUAL
  <|> string "=" *> pure EQUAL
  <|> string ">=" *> pure GREATER_EQUAL
  <|> string ">" *> pure GREATER
  <|> string "<=" *> pure LESS_EQUAL
  <|> string "<" *> pure LESS

parseComment :: Parser String Unit
parseComment = do 
  _ <- string "//"
  _<- anyTill (char '\n')
  pure unit

parseLiteral :: Parser String LoxToken
parseLiteral = 
  parseStringLiteral <|> parseNumberLiteral <|> parseIdentifier

parseStringLiteral :: Parser String LoxToken
parseStringLiteral = do 
  _ <- char '"'
  str <- many ((string "\\\"" *> pure '"') <|> noneOf ['"'])
  _ <- char '"'
  pure (STRING (fromCharArray <<< Array.fromFoldable $ str))

parseNumberLiteral :: Parser String LoxToken
parseNumberLiteral = do
  preDecimal <- many1 $ digit
  postDecimal <- try do 
    _ <- char '.'
    many $ digit
  let predec = toList preDecimal
  let z = (Number.fromString <<< fromCharArray <<< Array.fromFoldable) $ (predec <> (pure '.') <> postDecimal)
  num <- liftMaybe (\_ -> "Could not parse number") z
  pure $ NUMBER $ num

parseIdentifier :: Parser String LoxToken 
parseIdentifier = do
  head <- (noneOf $ toCharArray "\"'(){}[] =+-*/<>\n\t1234567890")
  tail <- (many (noneOf $ toCharArray "\"'(){}[] =+-*/<>\n\t"))
  pure $ IDENTIFIER $ fromCharArray $ Array.fromFoldable $ pure (head) <> (tail)
