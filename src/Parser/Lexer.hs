--import Text.Parsec.Token(makeTokenParser)
module Parser.Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef, LanguageDef)
import Parser.Syntax

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser def
          where 
            names = ["def", "print", "del", "pass", "break", "continue", "return", "raise", "import", "from", "exec", "assert"
                    , "if", "elif", "else", "while", "try", "finally", "with", "as", "or", "and", "not", "in", "is", "lambda"
                      , "class", "for", "yield"]
            def = emptyDef {
              Tok.commentLine  = "#"
              , Tok.identStart = letter <|> char '_'
              , Tok.identLetter = alphaNum <|> char '_'
              , Tok.opStart = oneOf ":;!#$%&*+./<=>?@\\^|-~"
              , Tok.reservedNames = names 
            }


parens :: Parser a -> Parser a
parens = Tok.parens lexer

commaSep :: Parser a -> Parser [a]
commaSep = Tok.commaSep lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Tok.commaSep1 lexer

semiSep :: Parser a -> Parser [a]
semiSep = Tok.semiSep lexer

semiSep1 :: Parser a -> Parser [a]
semiSep1 = Tok.semiSep1 lexer

identifier :: Parser String
identifier = Tok.identifier lexer

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

