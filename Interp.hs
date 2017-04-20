module Interp where

import System.IO
import SchemeAST
import SchemeParser

-- Seth Ahrenbach --
---
--- Values
---
data Value = I Int | R Float
           | ConsCell Value Value
           | NilVal
           | FunVal ([Value] -> Value) 
           
          

instance Show Value where
   show (I i)            = show i
   show (R r)            = show r
   show (ConsCell v1 NilVal) = show v1
   show (ConsCell v1 v2)  =  "(" ++ show v1 ++ body ++ ")"
                                where body = if (check v2) 
                                                then " " ++ nopshow v2
                                                else " . " ++ show v2
   show NilVal           = "()"
   show (FunVal f)       = "<function>"

-- HELPER FUNCTION nopshow doesn't include parens --

nopshow (ConsCell v1 NilVal) = show v1
nopshow (ConsCell v1 v2) = show v1 ++ " " ++ nopshow v2


-- Helper function check determines whether the ConsCell is a Scheme list or not --

check :: Value -> Bool

check NilVal = True
check (ConsCell v1 v2) = check v2
check _  = False    

---
--- Environments: using list implementation
---
type Env = [(String,Value)]

initEnv = []

applyEnv rho x = case (lookup x rho) of
                      Just v  -> v
                      Nothing -> error ("Variable "++x++" unbound\n")

extendEnv xs vs rho = newBindings ++ rho
       where newBindings = zip xs vs

-----------------------------------------------------
-----------------------------------------------------
------          The interpreter eval           ------
-----------------------------------------------------
-----------------------------------------------------

closure xs body rho = \ vs -> eval body (extendEnv xs vs rho) 

eval :: Term -> Env -> Value
eval (Litint i) rho  = I i
eval (Litreal r) rho = R r
eval (Var x) rho     = applyEnv rho x
eval Nil rho         = NilVal
eval Cons rho        = FunVal consVal
   where consVal :: [Value] -> Value
         consVal [v1,v2] = ConsCell v1 v2
         consVal _       = error "cons: too few or too many args!"
eval Plus rho 		 = FunVal saddl
eval Minus rho		 = FunVal ssubl
eval Times rho		 = FunVal smull		 
		 
eval (App [Cons, e1, e2]) rho  = let 
                                    FunVal cv = eval Cons rho
                                 in
                                    cv [eval e1 rho,eval e2 rho]
eval (App (Plus: es)) rho   = saddl (map (\t -> eval t rho) (es))

eval (App (Times : es)) rho = smull (map (\t -> eval t rho)(es))

eval (App (Minus : es)) rho = ssubl (map (\t -> eval t rho) (es))

eval t rho                     = error $ "Not defined yet: " ++ show t

--------------------------------------------------
--            Read-Eval-Print Loop              --
--------------------------------------------------

repl = top initEnv

top :: Env -> IO ()
top env = do
            putStr "TigerScheme> "
            iline <- getLine
            process env iline

process :: Env -> String -> IO ()
process env "quit" = return ()
process env iline  = case (parse iline) of
                          Just term -> 
                             putStr (" term = " ++ show term ++ "\n") >>
                             putStr (" val  = " ++ show v ++ "\n") >> top env
                                    where v = eval term env
                          Nothing   -> putStr "Syntax Error!\n" >> top env

						  
						 
-------------------------------------------------
--          Problem 2 Part (a)                 --
-------------------------------------------------

splus :: Value -> Value -> Value
splus x NilVal				= x
splus NilVal x			        = x
splus (I int1) (I int2)		        = I (int1 + int2)
splus (R float1) (R float2)	        = R (float1 + float2)
splus (I int) (R float)			= R ((int2float int) + float)
splus (R float) (I int)		        = R (float + (int2float int))
splus (ConsCell v1 v2)  (I int)  	= splus (splus v1 v2) (I int)
splus (ConsCell v1 v2)  (R float)       = splus (splus v1 v2) (R float)
splus (I int) (ConsCell v1 v2)	        = splus (splus (I int) v1) v2
splus (R float) (ConsCell v1 v2)        = splus (splus (R float) v1) v2
splus (ConsCell v1 v2) (ConsCell v3 v4) = splus (splus v1 v2) (splus v3 v4)


smult :: Value -> Value -> Value
smult x NilVal				= x
smult NilVal x				= x
smult (I int1) (I int2)			= I (int1 * int2)
smult (R float1) (R float2)		= R (float1 * float2)
smult (I int) (R float)			= R ((int2float int) * float)
smult (R float) (I int)			= R (float * (int2float int))
smult (ConsCell v1 v2) (I int)		= smult (smult v1 v2) (I int)
smult (ConsCell v1 v2) (R float)	= smult (smult v1 v2) (R float)
smult (I int) (ConsCell v1 v2)		= smult (smult (I int) v1) v2
smult (R float) (ConsCell v1 v2)	= smult (smult (R float) v1) v2
smult (ConsCell v1 v2) (ConsCell v3 v4) = smult (smult v1 v2) (smult v3 v4)

ssub :: Value -> Value -> Value
ssub x NilVal 		   = x
ssub NilVal x 		   = x
ssub (I int1) (I int2)	   = I (int1 - int2)
ssub (R float1) (R float2) = R (float1 - float2)
ssub (I int) (R float)	   = R ((int2float int) - float)
ssub (R float) (I int)	   = R (float - (int2float int))
  
						 
-------------------------------------------------
--	    Problem 2 Part (b)                 --
-------------------------------------------------		
saddl :: [Value] -> Value
saddl vs = foldr splus (I 0) vs

smull :: [Value] -> Value
smull vs = foldr smult (I 1) vs

ssubl :: [Value] -> Value
ssubl [] 			    = undefined
ssubl (v:vs) | (length (v:vs) == 1) = ssub (I 0) v
	     | otherwise            = foldl ssub (ssub v (I 0)) vs


------------------------------------------------
--        Problem 2 Part (c)                  --
--        See additions to eval               --
------------------------------------------------
				 
				 
------------------------------------------------
--           Problem 3                        --
------------------------------------------------
--          See additions to                  -- 
--          Show Instance of Value            -- 
--          above.                            --
------------------------------------------------      

