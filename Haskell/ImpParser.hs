module ImpParser where

import Parsing
import ImpAST

parseAdd    = do
                i1 <- parseImp
                symbol "+"
                i2 <- parseImp
                return (Add i1 i2)

parseExp    = do
                symbol "("
                i1 <- parseImp
                symbol "+"
                i2 <- parseImp
                symbol ")"
                return (Add i1 i2)

parseId  = do
             x <- identifier
             return (Var x)

parseInt = do 
             i <- integer
             return (Litint i)

parseAssign = do 
                x <- identifier
                symbol ":="
                i <- parseImp
                return (Assign x i)

parseSeq    = do symbol "("
                 asm <- parseImp
                 symbol ";"
                 asms <- parseImp 
                 symbol ")"
                 return (Seq asm asms)

parseImp = parseSeq +++ parseAssign +++ parseId +++ parseInt +++ parseExp

deP (P x) = x

parse input = case (deP parseImp input) of
                   ((imp,rest):_) -> Just imp
                   []             -> Nothing
