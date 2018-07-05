module Lexer
  ( toTokens
  )
where

import qualified Token                         as Tkn
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State

toTokens :: State String [(Tkn.TokenType, String)]
toTokens = do
  (t, s) <- nextToken
  case t of
    Tkn.EOF -> do
      put ""
      return [(t, s)]
    _ -> fmap ((t, s) :) toTokens

nextToken :: State String (Tkn.TokenType, String)
nextToken = do
  skipWhitespace
  x <- readChar
  case x of
    '=' -> do
      y <- peekChar
      case y of
        '=' -> do
          readChar
          return (Tkn.EQ, "==")
        _ -> return (Tkn.ASSIGN, show x)
    ';' -> return (Tkn.SEMICOLON, show x)
    '(' -> return (Tkn.LPAREN, show x)
    ')' -> return (Tkn.RPAREN, show x)
    ',' -> return (Tkn.COMMA, show x)
    '+' -> return (Tkn.PLUS, show x)
    '-' -> return (Tkn.MINUS, show x)
    '!' -> do
      y <- peekChar
      case y of
        '=' -> do
          readChar
          return (Tkn.NOT_EQ, "!=")
        _ -> return (Tkn.BANG, show x)
    '/'    -> return (Tkn.SLASH, show x)
    '*'    -> return (Tkn.ASTERISK, show x)
    '<'    -> return (Tkn.LT, show x)
    '>'    -> return (Tkn.GT, show x)
    '{'    -> return (Tkn.LBRACE, show x)
    '}'    -> return (Tkn.RBRACE, show x)
    '\x00' -> return (Tkn.EOF, show x)
    _
      | isLetter x -> fmap ((\l -> (Tkn.keywords l, l)) . (x :)) readIdentifier
      | isDigit x -> fmap (\l -> (Tkn.INT, x : l)) readIdentifier
      | otherwise -> return (Tkn.ILLEGAL, "")

readChar :: State String Char
readChar = do
  xs <- get
  case xs of
    []         -> return '\x00'
    (x : rest) -> do
      put rest
      return x

readNumber :: State String String
readNumber = do
  x <- peekChar
  if isDigit x then fmap (x :) readNumber else return ""

readIdentifier :: State String String
readIdentifier = do
  x <- peekChar
  if isLetter x then fmap (x :) readIdentifier else return ""

skipWhitespace :: State String ()
skipWhitespace = do
  xs <- get
  case xs of
    []         -> return ()
    (x : rest) -> when
      (isWhitespace x)
      (do
        put rest
        skipWhitespace
      )

peekChar :: State String Char
peekChar = do
  xs <- get
  case xs of
    []         -> return '\x00'
    (x : rest) -> return x

isLetter :: Char -> Bool
isLetter '_' = True
isLetter a | 'a' <= a && a <= 'z' = True
           | 'A' <= a && a <= 'Z' = True
           | otherwise            = False

isDigit :: Char -> Bool
isDigit a | '0' <= a && a <= '9' = True
          | otherwise            = False

isWhitespace :: Char -> Bool
isWhitespace ' '  = True
isWhitespace '\t' = True
isWhitespace '\n' = True
isWhitespace '\r' = True
isWhitespace _    = False
