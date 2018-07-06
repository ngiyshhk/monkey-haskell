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
        _ -> return (Tkn.ASSIGN, [x])
    ';' -> return (Tkn.SEMICOLON, [x])
    '(' -> return (Tkn.LPAREN, [x])
    ')' -> return (Tkn.RPAREN, [x])
    ',' -> return (Tkn.COMMA, [x])
    '+' -> return (Tkn.PLUS, [x])
    '-' -> return (Tkn.MINUS, [x])
    '!' -> do
      y <- peekChar
      case y of
        '=' -> do
          readChar
          return (Tkn.NOT_EQ, "!=")
        _ -> return (Tkn.BANG, [x])
    '/'    -> return (Tkn.SLASH, [x])
    '*'    -> return (Tkn.ASTERISK, [x])
    '<'    -> return (Tkn.LT, [x])
    '>'    -> return (Tkn.GT, [x])
    '{'    -> return (Tkn.LBRACE, [x])
    '}'    -> return (Tkn.RBRACE, [x])
    '\x00' -> return (Tkn.EOF, [x])
    _ | isLetter x -> (\l -> (Tkn.keywords l, l)) . (x :) <$> readIdentifier
      | isDigit x  -> (\l -> (Tkn.INT, x : l)) <$> readNumber
      | otherwise  -> return (Tkn.ILLEGAL, "")

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
  if isDigit x then (:) <$> readChar <*> readNumber else return ""

readIdentifier :: State String String
readIdentifier = do
  x <- peekChar
  if isLetter x then (:) <$> readChar <*> readIdentifier else return ""

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
