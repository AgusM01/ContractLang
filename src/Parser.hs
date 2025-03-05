module Parser where

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
-- en una variabble. 

-- let :: 'a' let | ... | 'z' let 
-- numVar ::= '0' numVar | ... | '9' numVar | let |eps 
-- num ::= '0' num | ... | '9' num
-- var ::= let num
-- DefContract ::=  var '=' ContExp | 
-- ContExp ::=  ContExp 'and' ContExp 
--              | ContExp 'or' ContExp 
--              | 'give' ContExp 
--              | 'truncate' var 
--              | 'then' ContExp ContExp
--              | 'scale' var ContExp  
--              | 'anytime' ContExp
--              | zero
--              | one var num 
--              | 'date' num num num


  

