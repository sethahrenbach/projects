module WhileAS where

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
