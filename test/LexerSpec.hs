module LexerSpec
  ( spec
  )
where

import           Test.Hspec
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Lexer
import           Token

spec :: Spec
spec = do
  describe "isDigit" $ do
    it "1 + 23 = 32" $ do
      (evalState toTokens "1 + 23 = 32")
        `shouldBe` [ (INT   , "1")
                   , (PLUS  , "+")
                   , (INT   , "23")
                   , (ASSIGN, "=")
                   , (INT   , "32")
                   , (EOF   , "\NUL")
                   ]
    it "let a = b" $ do
      (evalState toTokens "let a = b;")
        `shouldBe` [ (LET      , "let")
                   , (IDENT    , "a")
                   , (ASSIGN   , "=")
                   , (IDENT    , "b")
                   , (SEMICOLON, ";")
                   , (EOF      , "\NUL")
                   ]
    let txt = foldl
          (\acc o -> acc ++ "\n" ++ o)
          ""
          [ "let five = 5;"
          , "let ten = 10;"
          , ""
          , "let add = fn(x, y) {"
          , "  x + y;"
          , "};"
          , ""
          , "let result = add(five, ten);"
          , "!-/*5;"
          , "5 < 10 > 5;"
          , "if (5 < 10) {"
          , "  return true;"
          , "} else {"
          , "  return false;"
          , "}"
          , ""
          , "10 == 10;"
          , "10 != 9;"
          ]
    it txt $ do
      (evalState toTokens txt)
        `shouldBe` [ (LET      , "let")
                   , (IDENT    , "five")
                   , (ASSIGN   , "=")
                   , (INT      , "5")
                   , (SEMICOLON, ";")
                   , (LET      , "let")
                   , (IDENT    , "ten")
                   , (ASSIGN   , "=")
                   , (INT      , "10")
                   , (SEMICOLON, ";")
                   , (LET      , "let")
                   , (IDENT    , "add")
                   , (ASSIGN   , "=")
                   , (FUNCTION , "fn")
                   , (LPAREN   , "(")
                   , (IDENT    , "x")
                   , (COMMA    , ",")
                   , (IDENT    , "y")
                   , (RPAREN   , ")")
                   , (LBRACE   , "{")
                   , (IDENT    , "x")
                   , (PLUS     , "+")
                   , (IDENT    , "y")
                   , (SEMICOLON, ";")
                   , (RBRACE   , "}")
                   , (SEMICOLON, ";")
                   , (LET      , "let")
                   , (IDENT    , "result")
                   , (ASSIGN   , "=")
                   , (IDENT    , "add")
                   , (LPAREN   , "(")
                   , (IDENT    , "five")
                   , (COMMA    , ",")
                   , (IDENT    , "ten")
                   , (RPAREN   , ")")
                   , (SEMICOLON, ";")
                   , (BANG     , "!")
                   , (MINUS    , "-")
                   , (SLASH    , "/")
                   , (ASTERISK , "*")
                   , (INT      , "5")
                   , (SEMICOLON, ";")
                   , (INT      , "5")
                   , (Token.LT , "<")
                   , (INT      , "10")
                   , (Token.GT , ">")
                   , (INT      , "5")
                   , (SEMICOLON, ";")
                   , (IF       , "if")
                   , (LPAREN   , "(")
                   , (INT      , "5")
                   , (Token.LT , "<")
                   , (INT      , "10")
                   , (RPAREN   , ")")
                   , (LBRACE   , "{")
                   , (RETURN   , "return")
                   , (TRUE     , "true")
                   , (SEMICOLON, ";")
                   , (RBRACE   , "}")
                   , (ELSE     , "else")
                   , (LBRACE   , "{")
                   , (RETURN   , "return")
                   , (FALSE    , "false")
                   , (SEMICOLON, ";")
                   , (RBRACE   , "}")
                   , (INT      , "10")
                   , (Token.EQ , "==")
                   , (INT      , "10")
                   , (SEMICOLON, ";")
                   , (INT      , "10")
                   , (NOT_EQ   , "!=")
                   , (INT      , "9")
                   , (SEMICOLON, ";")
                   , (EOF      , "\NUL")
                   ]

