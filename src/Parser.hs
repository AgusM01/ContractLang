module Parser where
{-# HLINT ignore "Use <$>" #-}

import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Def 


totParser :: Parser a -> Parser a 
totParser p = do whiteSpace ctr 
                 t <- p 
                 eof 
                 return t 

-- Analizador de tokens
ctr :: TokenParser u 
ctr = makeTokenParser
    (emptyDef
        { commentStart = "/*"                                           ,
          commentEnd = "*/"                                             ,
          commentLine = "//"                                            ,
          opLetter = char '='                                           ,
          reservedNames = [ "zero", "one", "date", "USD", "EUR", "ARS"] , 
          reservedOpNames = [ "give", "and", "or", "truncate", 
                            "then", "scale", "get", 
                            "anytime", "=", ";" ]
        }
    )

-- Desambigüamos la gramática:
-- intexp ::= intexp '+' iterm | intexp '-' iterm | iterm 
-- iterm   ::= iterm '*' minus | iterm '/' minus | minus  
-- minus  ::= '-u' mm | mm  
-- mm     ::= nat | var | var '++' | var '--' | '('intexp')'  

 
-- Escribimos ahora la gramática del lenguaje
-- Para poner fechas t valores, estos deben estar previamente definidos
-- en una variable. 

-- let :: 'a' let | ... | 'z' let 
-- numVar ::= '0' numVar | ... | '9' numVar | let |eps 
-- num ::= '0' num | ... | '9' num
-- var ::= let num
-- Comm ::=  var '=' ContExp | Comm ';' Comm | var '=' Date

-- Date ::=  'date' num num num

-- Cur ::= "GBP" | "USD" | "ARS" | "EUR"
-- ContExp1 ::=  ContExp1 'and' ContExp2 
--              | ContExp1 'or' ContExp2 
--              | ContExp1 'then' ContExp2
--              | ContExp2

-- ContExp2 ::= | zero
--              | one var Cur
--              | 'give' ContExp2 
--              | 'truncate' var ContExp2
--              | 'scale' var ContExp2  
--              | 'anytime' ContExp2
--              | '(' ContExp1 ')'

contexp1 :: Parser Contract
contexp1 = chainl1 contexp2 op1Parser 

zeroParser :: Parser Contract 
zeroParser = do reserved ctr "zero"
                return Zero

oneParser :: Parser Contract
oneParser = try (do reserved ctr "one"
                    v <- varParser 
                    c <- curParser 
                    return $ OneV v c)
                <|> do reserved ctr "one"
                       d <- dateParser
                       c <- curParser
                       return $ OneD d c 

giveParser :: Parser (Contract -> Contract)
giveParser = do reserved ctr "give"
                return Give

truncateParser :: Parser (Contract -> Contract)
truncateParser = try (do reserved ctr "truncate"
                         v <- varParser 
                         return (TruncateV v))
                      <|> do reserved ctr "truncate"
                             d <- dateParser 
                             return (TruncateD d)

scaleParser :: Parser (Contract -> Contract)
scaleParser = try (do reserved ctr "scale"
                      n <- numParser
                      return (ScaleN n))
                 <|> do reserved ctr "scale"
                        v <- varParser 
                        return (ScaleV v)

anytimeParser :: Parser (Contract -> Contract)
anytimeParser = do reserved ctr "anytime"
                   return Anytime

                


{-
contextp2 :: Parser Contract 
contexp2 = try (do reserved lis "zero"
                   return Zero)
                <|> do reserved lis "one"
                       v <- var
                       c <- cur
                       return OneS v c 
                    <|> do reserved lis "one"
                           d <- date 
                           c <- cur 
                           return OneD d c
                        <|> do reserved lis "give"
                               c <- 
-}