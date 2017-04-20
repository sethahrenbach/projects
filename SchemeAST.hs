module SchemeAST where

data Term =
{- arithmetic language -}
    Litreal Float
  | Litint Int
  | Plus
  | Minus
  | Times
  | Div
{- functions -}
  | App [Term]
  | Lambda [String] Term
{- variable references -}
  | Var String
{- basic list ops -}
  | Cons 
  | Car 
  | Cdr  
  | List 
  | Nil
{- variable declarations -}
  | Letexp [(String,Term)] Term
  | Letstar [(String,Term)] Term
{- Booleans -}
  | TrueTerm
  | FalseTerm
{- Conditional expressions -} 
  | Equal
  | ITE Term Term Term
{- recursion -}
  | Letrec [(String,Term)] Term
{- dynamic binding -}
  | Letdyn [(String,Term)] Term
{- top-level define -}
  | Define String Term
{- loading a file -}
  | Load String 
     deriving Show

