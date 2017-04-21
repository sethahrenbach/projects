module SchemeParser where

import Parsing
import SchemeAST

builtinSymbol = symbol "+" 
                +++ symbol "-" 
                +++ symbol "*" 
                +++ symbol "/" 
                +++ symbol "cons"
                +++ symbol "car"
                +++ symbol "cdr"
                +++ symbol "()"
                +++ symbol "list"
                +++ symbol "="
                +++ symbol "#t"
                +++ symbol "#f"

parseOp = do
            isym <- builtinSymbol
            return (tr isym)
               where 
                  tr "+"    = Plus
                  tr "-"    = Minus
                  tr "*"    = Times
                  tr "/"    = Div
                  tr "cons" = Cons
                  tr "car"  = Car
                  tr "cdr"  = Cdr
                  tr "()"   = Nil
                  tr "list" = List
                  tr "="    = Equal
                  tr "#t"   = TrueTerm
                  tr "#f"   = FalseTerm

parseId  = do
             x <- identifier
             return (Var x)

parseInt = do 
             i <- integer
             return (Litint i)

parseReal = do 
              i <- integer
              symbol "."
              j <- integer
              return (Litreal (ints2float i j))

int2float :: Int -> Float
int2float = fromIntegral 

ints2float i j = int2float i + foo j

--- This is hideous.
foo 0 = 0.0
foo x = if x<0 then error "No negatives!!!"
               else (int2float x) / bar x

bar :: Int -> Float
bar x = power exponent 10
     where log10    = logBase 10 (int2float x)
           clog10   = ceiling log10
           exponent = if int2float clog10 == log10 
                         then clog10+1 
                         else clog10

power :: Int -> Float -> Float
power 0 x = 1
power n x = x * power (n-1) x

parseAtom = parseOp +++ parseReal +++ parseInt +++ parseId

parseSexp = do
              symbol "("
              es <- many parseExp
              symbol ")"
              return (App es)

parseBinding = do 
                 symbol "("
                 x <- identifier
                 e <- parseExp
                 symbol ")"
                 return (x,e)

parseLetStar = do
                 symbol "("
                 symbol "let*"
                 symbol "("
                 bs <- many parseBinding
                 symbol ")"
                 body <- parseExp
                 symbol ")"
                 return (Letstar bs body)

parseLetrec  = do
                 symbol "("
                 symbol "letrec"
                 symbol "("
                 bs <- many parseBinding
                 symbol ")"
                 body <- parseExp
                 symbol ")"
                 return (Letrec bs body)

parseLet     = do
                 symbol "("
                 symbol "let"
                 symbol "("
                 bs <- many parseBinding
                 symbol ")"
                 body <- parseExp
                 symbol ")"
                 return (Letexp bs body)

parseLambda = do
                symbol "("
                symbol "lambda"
                symbol "("
                xs <- many identifier
                symbol ")"
                body <- parseExp
                symbol ")"
                return (Lambda xs body)

parseDefine  = do
                 symbol "("
                 symbol "define"
                 x    <- identifier
                 body <- parseExp
                 symbol ")"
                 return (Define x body)

parseLoad   = do
                 symbol "("
                 symbol "load"
                 file   <- identifier
                 symbol ")"
                 return (Load file)

parseITE    = do
                symbol "("
                symbol "if"
                test <- parseExp
                tbr  <- parseExp
                fbr  <- parseExp
                symbol ")"
                return (ITE test tbr fbr)
       
parseExp = parseAtom 
               +++ parseLambda 
               +++ parseLetStar
               +++ parseLetrec
               +++ parseLet
               +++ parseDefine
               +++ parseLoad
               +++ parseITE
               +++ parseSexp

deP (P x) = x

parse input = case (deP parseExp input) of
                   ((term,rest):_) -> Just term
                   []              -> Nothing
