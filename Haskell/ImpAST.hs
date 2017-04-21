module ImpAST where

type Loc = String

data Imp = Assign Loc Imp
         | Seq Imp Imp 
         | Litint Int 
         | Add Imp Imp 
         | Var String
         -- add negation
         | Negate Imp
         | DividedBy Imp Imp
         | Subtract Imp Imp
         | Times Imp Imp
  
instance Show Imp where
    show (Assign x i) = x ++ ":=" ++ show i
    show (Seq c1 c2)  = "("++ show c1 ++ " ; " ++ show c2 ++ ")"
    show (Litint i)   = show i
    show (Add i1 i2)  = "(" ++ show i1 ++ "+" ++ show i2 ++ ")"
    show (Var v)      = v
    -- add show for negation
    show (Negate i)   = "(" ++ "-" ++ show i ++ ")"