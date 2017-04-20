module ImpFrontEnd where

--import Prelude hiding (Maybe,Nothing,Just)
import System.IO
import ImpAST
import ImpParser

repl :: IO ()
repl = do
         putStr "Imp> "
         iline <- getLine
         process iline

process :: String -> IO ()
process "quit" = return ()
process iline  = case (parse iline) of
                      Just imp -> 
                         putStr (show imp ++ 
                                 "\n"++" final value = " ++ 
                                   show v ++ "\n"++ 
                                   " final store = " ++ show s ++ 
                                   "\n") >> repl
                                where (v,s) = exec imp initStore
                      Nothing   -> putStr "Syntax Error!\n" >> repl

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

{-
data Maybe a = Just a | Nothing

instance Monad Maybe where
   return v       = Just v
   (Just v) >>= f = f v
   Nothing >>= f  = Nothing
   x >>= f = case x of 
                  Just v  -> f v
                  Nothing -> Nothing
-}  
throw = Nothing

initStore  = Mem []

ex :: Imp -> Store -> Maybe (Value,Store)
ex (Assign l imp) (Mem s) {- = case ex imp (Mem s) of
                                 Just (I i,Mem s') -> return (NilVal,Mem ((l,i) : (dropfst l s')))
                                 Nothing           -> Nothing -}
                          = ex imp (Mem s) >>= \ (I i,Mem s') -> return (NilVal,Mem ((l,i) : (dropfst l s')))
                                 
ex (Seq c1 c2) mem0      {- = case ex c1 mem0 of
                                 Just (_,mem1) -> ex c2 mem1
                                 Nothing       -> Nothing -}
                          = ex c1 mem0 >>= \ (_,mem1) -> ex c2 mem1
                                                         
ex (Litint i) mem0      {- Just (I i,mem0) -}
                          = return (I i,mem0)
ex (DividedBy i1 i2) mem0 =  ex i1 mem0 >>= \ (I v1,mem1) ->
                             ex i2 mem1 >>= \ (I v2,mem2) ->
			        case v2 of
                                     0 -> throw
                                     _ -> return (I (v1`div`v2),mem2)
                              {- let
				Just (I v1,mem1) = ex i1 mem0
				Just (I v2,mem2) = ex i2 mem1
			    in
			        case v2 of
                                     0 -> Nothing
                                     _ -> Just (I (v1`div`v2),mem2)
                            -}
                           
                            
ex (Var x) (Mem s) = {-  (I v,Mem s)
    where v = case (lookup x s) of
                   Just v  -> v
                   Nothing -> 0 -- error ("Variable "++x++" not found")  -}
                     (lookup x s) >>= \ v -> return (I v,Mem s)

ex (Add i1 i2) mem0       = {- let
                                  (I v1,mem1) = exec i1 mem0
                                  (I v2,mem2) = exec i2 mem1
                              in
                                  (I (v1+v2),mem2) -}
                            ex i1 mem0 >>= \ (I v1,mem1) ->
                            ex i2 mem1 >>= \ (I v2,mem2) ->
                            return (I (v1+v2),mem2)

ex (Negate i) mem0        = {- let 
		  	          (I v, mem1) = exec i mem0
		              in
			          (I (-v), mem1) 

  -}                        ex i mem0 >>= \ (I v, mem1) -> return (I (-v), mem1)        
ex (Subtract i1 i2) mem0  = {- let
                                  (I v1,mem1) = exec i1 mem0
                                  (I v2,mem2) = exec i2 mem1
                              in
                                  (I (v1-v2),mem2)
-}                           ex i1 mem0 >>= \ (I v1,mem1) ->
                             ex i2 mem1 >>= \ (I v2,mem2) ->
                             return (I (v1-v2),mem2)

ex (Times i1 i2) mem0     = {- let
				  (I v1,mem1) = exec i1 mem0
				  (I v2,mem2) = exec i2 mem1
			      in 
			  	  (I (v1*v2),mem2) -}
                             ex i1 mem0 >>= \ (I v1,mem1) ->
                             ex i2 mem1 >>= \ (I v2,mem2) ->
                             return (I (v1*v2),mem2)

exec :: Imp -> Store -> (Value,Store)
exec (Assign l imp) (Mem s)
               = (NilVal,Mem ((l,i) : (dropfst l s')))
       where (I i,Mem s') = exec imp (Mem s)
exec (Seq c1 c2) mem0  
               = let 
                     (_,mem1) = exec c1 mem0
                 in 
                     exec c2 mem1
exec (Var x) (Mem s) = (I v,Mem s)
    where v = case (lookup x s) of
                   Just v  -> v
                   Nothing -> 0 -- error ("Variable "++x++" not found")
exec (Litint i) mem0        = (I i,mem0)
exec (Add i1 i2) mem0       = let
                                  (I v1,mem1) = exec i1 mem0
                                  (I v2,mem2) = exec i2 mem1
                              in
                                  (I (v1+v2),mem2)
-- add case for negation
exec (Negate i) mem0        = let 
		  	          (I v, mem1) = exec i mem0
		              in
			          (I (-v), mem1) 
-- add case for subtraction
exec (Subtract i1 i2) mem0  = let
                                  (I v1,mem1) = exec i1 mem0
                                  (I v2,mem2) = exec i2 mem1
                              in
                                  (I (v1-v2),mem2)
-- add case for multiplication
exec (Times i1 i2) mem0     = let
				  (I v1,mem1) = exec i1 mem0
				  (I v2,mem2) = exec i2 mem1
			      in 
			  	  (I (v1*v2),mem2)
-- add case for division
exec (DividedBy i1 i2) mem0 = let
				  (I v1,mem1) = exec i1 mem0
				  (I v2,mem2) = exec i2 mem1
			      in
			          (I (v1`div`v2),mem2)
			          
dropfst x [] = []
dropfst x ((y,v):rs) = if x==y 
                         then dropfst x rs
                         else (y,v) : dropfst x rs


{- fahrcel :: Int -> Int

fahrcel n = 5 * (n-32) `div` 9

celfahr :: Int -> Int

celfahr n = ((n*9)`div`5)+32 -}