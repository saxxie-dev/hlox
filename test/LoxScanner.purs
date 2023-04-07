module Test.LoxScanner where 

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Array as Array
import Data.Array.NonEmpty as List
import Data.Either (Either(..), fromRight)
import Data.Identity (Identity(..))
import Data.List (List(..))
import Data.Tuple (snd)
import Effect.Aff (Error)
import LoxScanner (LoxToken(..), loxScanner, parseComment, parseKeyword, parseLiteral, parseSpecialCharToken)
import Parsing (ParseState(..), Position(..), runParser, runParserT')
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)


loxScannerTests :: forall m1 g2. Monad m1 => MonadThrow Error g2 => SpecT g2 Unit m1 Unit
loxScannerTests = 
  describe "LoxScanner" do
    describe "keywordParser" do
      it "Should parse a single keyword in isolation" do 
        (runParser "and" parseKeyword) `shouldEqual` (Right AND)
      it "Should fail to parse an invalid keyword in isolation" do 
        (runParser "hello" parseKeyword) `shouldSatisfy` (\x -> case x of
          Left _ -> true
          Right _ -> false)
      it "Should be able to parse each of the keywords" do 
        ((\x -> fromRight COMMA (runParser x parseKeyword)) <$> ["and", "class", "else", "false", "fun", "for", "if", "nil", "or", "print", "return", "super", "this", "true", "var", "while"])
          `shouldEqual` [AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE]
    describe "specialCharParser" do
      it "Should preferrentially parse != instead of !" do
        (runParser "!=" parseSpecialCharToken) `shouldEqual` (Right BANG_EQUAL)
    describe "commentParser" do 
      it "Should consume comment only" do 
        case (
          runParserT'
          (ParseState "//yo\nthere" (Position{index:0, line:0, column:0}) false)
          parseComment
          ) of 
            Identity (x) -> case snd x of 
              ParseState y _ _ -> y `shouldEqual` "there"
    describe "literalParser" do
      it "Should parse a simple string" do 
        (runParser "\"hello world\"" parseLiteral) `shouldEqual` (Right $ STRING $ "hello world")
      it "Should parse a string with escaped quotes" do 
        (runParser "\"he said \\\"Hi!\\\"\"" parseLiteral) `shouldEqual` (Right $ 
          STRING "he said \"Hi!\"")
      it "Should parse a simple number" do 
        (runParser "3.14" parseLiteral) `shouldEqual` (Right $ NUMBER $ 3.14)
      it "Should parse an identifier" do 
        (runParser "secretNum35 = 35" parseLiteral) `shouldEqual`
          (Right $ IDENTIFIER "secretNum35")
    it "Should correctly parse a simple program" do 
      (Array.fromFoldable <$> runParser """
        class Brunch < Breakfast {
          drink() {
            print "How about a Bloody Mary?";
          }
        }""" loxScanner) `shouldEqual` (Right 
          [CLASS,(IDENTIFIER "Brunch"),LESS,
          (IDENTIFIER "Breakfast"),LEFT_BRACE,
          (IDENTIFIER "drink"),LEFT_PAREN,
          RIGHT_PAREN,LEFT_BRACE,PRINT,
          (STRING "How about a Bloody Mary?"),
          SEMICOLON,RIGHT_BRACE,RIGHT_BRACE])
