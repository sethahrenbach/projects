module Homework4 where

import WhileAS
import While 
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( javaStyle )

{-
Homework4, CS4450/7450
Out: October 22nd, 2012
Due: October 29th by 3pm Central time.

Directions. Throughout this file, there are four problems each worth 25 points. Answer each 
in this file. Be sure to test your answers. Email your solution (i.e., the file Homework4.hs)
to Ian Graves (iangraves@mail.mizzou.edu).

In order to receive any (and I do mean "any") credit for your solutions, your solution must
load into GHCi without errors.

-}

{-
Problem 1. Define instances of the Show class for Stat, AExp and BExp
-}


instance Show BExp where
  show (BUnOp op b1)    = op ++ show (b1)
  show (BoolLit True)   = "(TRUE)"
  show (BoolLit False)  = "(FALSE)"
  show (RelOp op e1 e2) = show (e1) ++ op ++ show (e2)

instance Show Stat where
  show (Assign x e _)           = x ++ " " ++ ":=" ++ " " ++ show (e)
  show (Skip _)                 = "SKIP"
  show (Seq (x:xs)) |(xs == []) = show (x)
                    |otherwise  = show (x) ++ "; " ++ show (Seq (xs))
  show (While b _ s)            = "while " ++ show (b) ++ " " ++ "DO " ++ "(" ++ show (s) ++ ")"
  show (NewInt x s)             = "newint " ++ show (x) ++ " " ++ show (s)
  show (If b _ s1 s2)           = "if " ++ show (b) ++ ", then " ++ show (s1) ++ ", " ++ "else " ++ show (s2)

instance Show AExp where
  show (Var x)        = x
  show (IntLit e)     = show (e)
  show (AOp op e1 e2) = show (e1) ++ " " ++ op ++ " " ++ show (e2)

{-
Problem 1 (continued). Test your answer with the function defined below.
-}
front fname
  = do{ input <- readFile fname
      ; putStr input
      ; case parse program fname input of
           Left err -> error (show err)
           Right x  -> return x
      }

{-
Problem 2. Below is the interpreter for the simple imperative language which we discussed in
class. There are several questions inserted in the code below. Answer them by extending 
the interpreter. Be sure to test your answers.
-}

type Memory = [(Ident, Int)]
type Trans = (Stat, Memory) -> Memory

--
-- memory look-up
--
lkup :: Memory -> Ident -> Int
lkup ((x, n):ms) x' = if x == x' then n else lkup ms x'
lkup [ ] x'         = error ("Unbound variable: " ++ x')

--
-- evaluate an arithmetic expression:
--
evE (Var i) m        = lkup m i
evE (IntLit i) m     = i
evE (AOp op e1 e2) m = case op of
                         "+" -> evE e1 m + evE e2 m
                         "*" -> evE e1 m * evE e2 m
                         "-" -> evE e1 m - evE e2 m
                         _   -> error ("Undefined operator: " ++ op)
                            
evB (BUnOp "not" b) m    = not (evB b m)
evB (BoolLit b) m        = b
evB (BOp "&" b1 b2) m    = evB b1 m && evB b2 m
evB (RelOp ">" e1 e2) m  = evE e1 m > evE e2 m
evB (RelOp "<=" e1 e2) m = evE e1 m <= evE e2 m

exec :: Stat -> Memory -> Memory
exec (Skip _) m       = m
exec (Assign i e _) m = (i,evE e m) : m
-- 
-- Problem 2 a.
-- The definition below for Seq handles sequences of statements of precisely length 2.
-- Redefine this clause for the general case in which there are arbitrary numbers of 
-- statements.
--
exec (Seq []) m       = m
exec (Seq (c:cs)) m   = exec (Seq cs) (exec c m)

exec (If b _ s1 s2) m = if evB b m
                           then
                              exec s1 m
                           else
                              exec s2 m
exec (While b l c) m  = if evB b m 
                           then
                              exec (Seq [c,While b l c]) m
                           else
                              m



-- 
-- Problem 2 b.
-- The (NewInt x c) statement defines a variable x that may be used exclusively within c.
-- The effect of (NewInt x c) on the memory m should return a new memory m' in which
-- x is initialized to 0 (but is otherwise identical to m).
--
-- Define NewInt below.
--  type Memory = [(Ident, Int)]


exec (NewInt x c) m  = exec c ((x, 0) : m)
                    


mem0 = [("ans",0)]
test fname
  = do{ input <- readFile fname
      ; putStr input
      ; case parse program fname input of
           Left err -> error (show err)
           Right x  -> return (exec x mem0)
      }


{- Problem 3.
A static check commonly used in language implementations determines if all variables used 
in the program are declared by a NewInt first. For an example, look at fib.wh and newfib.wh. 

// fib.wh uses undeclared variables.
    v := 1; 
    u := 1; 
    if n <= 2 then 
      skip 
    else 
      while n > 2 do (
        t := u; 
        u := v; 
        v := u + t
        )

// newfib.wh declares all variables that it uses.
    newint v in (
    newint u in (
           v := 1; 
           u := 1; 
       newint n in (
         newint t in (
           if n <= 2 then 
             skip 
           else 
             while n > 2 do (
    	       t := u; 
    	       u := v; 
    	       v := u + t
         )
   ))
))
{- 
type  Ident = String
type  Label = Int
  
data AExp 
  = Var Ident 
  | IntLit Int
  | AOp String AExp AExp
  deriving Eq

data BExp 
  = BUnOp String BExp
  | BoolLit Bool
  | BOp String BExp BExp
  | RelOp String AExp AExp
  deriving Eq

data Stat
  = Assign Ident AExp Label
  | Skip Label
  | Seq [Stat]
  | If BExp Label Stat Stat
  | While BExp Label Stat
-- The new statement for defining variables:
  | NewInt Ident Stat
  deriving Eq
-}

    
Below is a code outline for this static check. (checkS s []) is True if s contains no
undeclared variables. (check (NewInt x c) ds) records in ds that x is declared when it 
recursively checks c. Complete the definitions of checkS, checkE and checkB. Test your 
solution with check below. In particular, (check "newfib.wh") should be True and 
(check "fib.wh") should be False.
-}

checkS :: Stat -> [Ident] -> Bool

checkS c r            = case c of 
                             (Skip _)       -> True
                             (Assign x e _) -> checkE (Var x) r && checkE e r
                             (Seq (c:cs))   -> if (cs /= []) then (checkS (Seq cs) r) && (checkS c r) else (checkS c r)
                             (If b _ s1 s2) -> checkB b r && checkS s1 r && checkS s2 r
                             (While b _ c)  -> checkB b r && checkS c r
                             (NewInt i c)   -> checkS c (i : r)

--if elem ("newint" ++ i) r then checkS c (i : r) else if elem i r then True else False

checkE :: AExp -> [Ident] -> Bool
checkE e r = case e of 
                  (Var x)        -> elem x r
                  (IntLit i)     -> True
                  (AOp op e1 e2) -> (checkE e1 r) && (checkE e2 r)
                  
checkB :: BExp -> [Ident] -> Bool
checkB b r = case b of
                  (BUnOp "not" b)    -> checkB b r
                  (BoolLit b)        -> True
                  (BOp "&" b1 b2)    -> checkB b1 r && checkB b2 r
                  (RelOp ">" e1 e2)  -> checkE e1 r && checkE e2 r
                  (RelOp "<=" e1 e2) -> checkE e1 r && checkE e2 r


check fname
  = do{ input <- readFile fname
      ; putStr input
      ; case parse program fname input of
           Left err -> error (show err)
           Right x  -> return (checkS x [])
      }

{-
Problem 4. Write a read-eval-print loop that (a) reads a file name from the prompt, (b) 
calls the parser on it, (c) runs checkS of the program, and, (d) if there are no undeclared 
variables, runs the exec interpreter on it. Finally, the loop should start again. Your 
solution will look something like the check function above. Consult the lecture slides
for more information about read-eval-print loops.
-}

repl = do
           putStr "Enter a program for me to check: "
           xs <- getLine
           input <- readFile xs
           putStr input 
           case parse program xs input of 
              Left err -> error (show err)
              Right x -> do 
                            if (checkS x []) then putStr (show (exec x []) ++ "\n") else putStr (show "Sorry, that has undeclared variables! Try another..." ++ "\n")
                            repl
           
                
        

