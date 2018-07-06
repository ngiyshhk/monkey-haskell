module Token
  ( TokenType(..)
  , keywords
  )
where

data TokenType = ILLEGAL | EOF | IDENT | INT |
  ASSIGN | PLUS | MINUS | BANG | ASTERISK | SLASH |
  LT | GT | COMMA | SEMICOLON | LPAREN | RPAREN |
  LBRACE | RBRACE | FUNCTION | LET | TRUE | FALSE |
  IF | ELSE | RETURN | EQ | NOT_EQ deriving (Show, Eq)

keywords :: String -> TokenType
keywords "fn"     = FUNCTION
keywords "let"    = LET
keywords "true"   = TRUE
keywords "false"  = FALSE
keywords "if"     = IF
keywords "else"   = ELSE
keywords "return" = RETURN
keywords _        = IDENT
