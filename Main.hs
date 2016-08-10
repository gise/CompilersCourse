module Main where

import Lang
import Parser (parser)
import Lexer (lexer)

parse :: String -> Comm
parse prg = case lexer prg of
              Left _ -> error "Error en el lexer"
              Right ts -> case parser ts of
                            Left _ -> error "Error en el parser"
                            Right c -> c