module HW3 where

-- By Seth Ahrenbach
{-
Directions. It is important that you follow these directions exactly.
Please understand that this is a large class and letting every one do
their own thing puts a tremendous burden on the TA. The questions begin
on line 115 of this file.

General Requirements.
1. Your solutions to these questions are to be included in this file only.
2. Turn in your homework by emailing them to the TA, Ian Graves, with
   the subject line "HW3" as an attachment.
3. IMPORTANT: your homework must load into ghci without errors to receive
   any credit. That is, if what you send to Ian does not load into ghci
   then you will receive a zero for this assignment. No exceptions.
4. If you don't finish a problem and have something that you'd like to have
   considered as partial credit, put it within a comment. This isn't a 
   guarantee that you'll get partial credit, though.
5. Type declarations must be included with every definition you give.

Advice. Remember the type template idea as you proceed.

Due date: Friday, 9/28 by 3pm.
-}

type Var = String

data Prop = Atom Var 
          | Not Prop
          | Imply Prop Prop
            
{-            
Recall the derivation of ( - p ):
   Prop ->
     ( - Prop )
     ( - p )

We can represent this as the following Haskell value:
-}

negp = Not (Atom "p")

instance Show Prop where
  show (Atom p)            = p
  show (Not prop)          = "(-" ++ show prop ++ ")"
  show (Imply prop1 prop2) = "(" ++ show prop1 ++ " => " ++ show prop2 ++ ")"

instance Eq Prop where
  (Atom p) == (Atom q)       = p == q
  (Not x) == (Not y)         = x == y
  (Imply x y) == (Imply u v) = (x == u) && (y == v)
  _ == _                     = False
  
--   
-- Connectives  
--  
  
orPL :: Prop -> Prop -> Prop
orPL phi gamma  = Imply (Not phi) gamma
andPL phi gamma = Not (orPL (Not phi) (Not gamma))
iffPL phi gamma = andPL (Imply phi gamma) (Imply gamma phi)
  
--
-- Axioms
--

-- ...as functions

axiom1 :: Prop -> Prop -> Prop
axiom1 phi gamma     = Imply phi (Imply gamma phi)

axiom2 :: Prop -> Prop -> Prop -> Prop
axiom2 phi gamma psi = Imply pre post
   where pre  = Imply phi (Imply gamma psi)
         post = Imply
                   (Imply phi gamma)
                   (Imply phi psi)

axiom3 phi gamma = Imply pre post
   where pre  = Imply (Not gamma) (Not phi)
         post = Imply hyp gamma
            where hyp  = Imply (Not gamma) phi

-- ...as data type

data Axiom = Ax1 Prop Prop
           | Ax2 Prop Prop Prop
           | Ax3 Prop Prop
             deriving Eq
                       
instance Show Axiom where
  show (Ax1 phi gamma)     = show (axiom1 phi gamma)
  show (Ax2 phi gamma psi) = show (axiom2 phi gamma psi)
  show (Ax3 phi gamma)     = show (axiom3 phi gamma)

data Theorem = AxiomInst Axiom | ModusPonens Theorem Theorem Prop


------ BEGIN REPLACEMENT CODE FOR SHOW ------
indent s = "     ," ++ ind s
  where ind ('\n':s') = "\n" ++ "     |" ++ ind s'
        ind (c:s')    = c : ind s'
        ind ""        = ""

instance Show Theorem where
  show (AxiomInst ax@(Ax1 _ _))   = "---- " ++ show ax ++ "    [Axiom 1]"
  show (AxiomInst ax@(Ax2 _ _ _)) = "---- " ++ show ax ++ "    [Axiom 2]"
  show (AxiomInst ax@(Ax3 _ _))   = "---- " ++ show ax ++ "    [Axiom 3]"
  show (ModusPonens t1 t2 c)      = indent (
                                        show t1
                                     ++ "\n\n\n"
                                     ++ show t2
                                     ++ "\n\n")
                                 ++ "\n---- " ++ show c ++ "   [Modus Ponens]"
------ END REPLACEMENT CODE FOR SHOW ------


{- instance Show Theorem where
  show (AxiomInst ax)             = show ax
  show (ModusPonens x y z) 
     =         show x ++ "   " ++ show y
       ++ "\n--------------------------------\n    " ++
                       show z -}

-- Subproof of A => A

a = Atom "A"
b = Atom "B"                                            -- Note: I added "B" and "C" --
c = Atom "C"

subproof = ModusPonens hyp1 hyp2 conc
  where hyp1 = AxiomInst (Ax1 a (Imply a a))
        hyp2 = AxiomInst (Ax2 a (Imply a a) a)
        conc = Imply (Imply a (Imply a a))
                     (Imply a a)

{-
Question 1. Represent the proof on Slide 42 of the "Propositional Logic
in Haskell" slides by complete the definition below. You may use subproof, defined
about.
-}

-- data Theorem = AxiomInst Axiom | ModusPonens Theorem Theorem Prop

slide42 :: Theorem
slide42 = ModusPonens (AxiomInst (Ax1 a a)) prem2 (Imply a a)
              where prem2 = subproof

{-
Question 2. The modus ponens rule is:
     p    p => q
   ----------------
          q

If we have 3 Prop values (x,y,z :: Prop), then saying that z follows from 
x and y by modus ponens means three things:
   1. y has the form (Imply u w)
   2. u==x
   3. w==z
Written in Haskell, 1.-3. become:
-}

mp :: Prop -> Prop -> Prop -> Bool
mp x (Imply u w) z = u==x && w==z
mp _ _ _           = False

{-
So, the function (mp x y z) is True if, and only if, z follows from x and y by 
modus ponens.

{- data Prop = Atom Var 
          | Not Prop
          | Imply Prop Prop -}

Provide definitions for p1, p2, p3, q1, q2, and q3 for which
(a) (mp p1 p2 p3) is True
(b) (mp q1 q2 q3) is False
-}

p = Atom ("P")                                          -- Note: I added "P" and "Q" --
q = Atom ("Q")

p1 = p          
p2 = Imply p q 
p3 = q

q1 = p
q2 = q
q3 = Imply q p   -- this is the fallacy of affirming the consequent.

{-
Question (3). Write a function, thm2prop, that takes a Theorem and returns the Prop that 
is the conclusion of the theorem. So, for example, for subproof above,
   thm2prop subproof 
will return (in pretty printed form):
   Imply (Imply a (Imply a a)) (Imply a a)
This will look at the prompt:
   ((A => (A => A)) => (A => A))

Hint: you may use functions above to help you with this.
-}

thm2prop :: Theorem -> Prop
thm2prop (AxiomInst ax)      = case ax of
                                    (Ax1 d e)           -> axiom1 d e
                                    (Ax2 d e f)         -> axiom2 d e f 
                                    (Ax3 d e)           -> axiom3 d e
thm2prop (ModusPonens prm1 prm2 c) = c

{-
Now we can write an analogous function to mp, except it looks at
(the conclusions of) theorems.
-}

mpthm :: Theorem -> Theorem -> Prop -> Bool
mpthm t1 t2 c = mp (thm2prop t1) (thm2prop t2) c

{-
Question (4). Write a recursive function, chkthm, that takes a Theorem and
ensures that every application of modus ponens within it is valid, returning
True if every application is valid and False otherwise. For a Theorem t,
(chkthm t) will be True for any axiom instance. If t is (ModusPonens p q c), 
then this will be True if, and only if, 
   1. c follows from p and q by modus ponens *and* 
   2. all the applications of modus ponens are valid within p and q both.

Hint: check 2. with a recursive call.
-}

chkthm :: Theorem -> Bool

chkthm (AxiomInst ax) = True
chkthm (ModusPonens thm1 thm2 prp) = ((chkthm thm1) && (chkthm thm2)) && ((mpthm thm1 thm2 prp)||(mpthm thm2 thm1 prp))



badthm = ModusPonens
          (AxiomInst (Ax1 (Atom "a") (Atom "b")))
          (AxiomInst (Ax2 (Atom "a") (Atom "b") (Atom "b")))
          (Imply (Atom "a") (Atom "b"))


okthm = ModusPonens
          slide42
          (AxiomInst (Ax1 (Imply a a) (Atom "B")))
          (Imply (Atom "B") (Imply a a))

