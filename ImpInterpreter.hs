module ImpInterpreter where

-- Abstract Syntax
type Loc = String

-- Note: this is richer than the language
-- discussed in class: Locs can be assigned
-- Imps, not just constant Ints.
data Imp = Assign Loc Imp
         | Seq Imp Imp 
         | Litint Int 
         | Add Imp Imp 
         | Var String

instance Show Imp where
    show (Assign x i) = x ++ ":=" ++ show i
    show (Seq c1 c2)  = "("++ show c1 ++ " ; " ++ show c2 ++ ")"
    show (Litint i)   = show i
    show (Add i1 i2)  = "(" ++ show i1 ++ "+" ++ show i2 ++ ")"
    show (Var v)      = v


-----------------------------------------------------
-----------------------------------------------------
------          The interpreter exec           ------
-----------------------------------------------------
-----------------------------------------------------

data Store = Mem [(Loc,Int)] deriving Show
data Value = I Int | NilVal

instance Show Value where
   show (I i)  = show i
   show NilVal = "()"

initStore  = Mem []

exec :: Imp -> Store -> (Value,Store)
exec (Assign l imp) (Mem s) = (NilVal,Mem ((l,i) : (dropfst l s')))
       where (I i,Mem s') = exec imp (Mem s)
exec (Seq c1 c2) mem0       = let 
                                 (_,mem1) = exec c1 mem0
                              in 
                                 exec c2 mem1
exec (Var x) (Mem s) = (I v,Mem s)
    where v = case (lookup x s) of
                   Just v  -> v
                   Nothing -> error ("Variable "++x++" not found")

exec (Litint i) mem0  = (I i,mem0)
exec (Add i1 i2) mem0 = let
                           (I v1,mem1) = exec i1 mem0
                           (I v2,mem2) = exec i2 mem1
                        in
                           (I (v1+v2),mem2)
dropfst x [] = []
dropfst x ((y,v):rs) = if x==y 
                         then dropfst x rs
                         else (y,v) : dropfst x rs

-- Examples

i0 = Seq (Assign "x" (Litint 3)) (Var "x")
i1 = Assign "x" i0
