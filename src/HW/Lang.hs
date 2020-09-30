module HW.Lang where

import Control.Applicative
import Control.Monad

-- type of identifiers
type Ident = String

-- a builtin function
data Builtin = Print
             | Input
             | Str
             | Int
             deriving (Eq, Show)

-- a value of a variable
data Value = VInt Int
           | VBool Bool
           | VString String
           | VDef [Ident] Block
           | VBuiltin Builtin
           | VNone
           deriving (Eq, Show)

-- binary operation
data Binop = Add
           | Sub
           | Mul
           | Div
           | Mod
           | Eq
           | Ne
           | Gt
           | Ge
           | Lt
           | Le
           | And
           | Or
           deriving (Eq, Show, Enum)

-- unary operation
data Unop = Not
          | Neg
           deriving (Eq, Show, Enum)

-- expression type
data Expr = Lit Value -- literal
          | Ref Ident -- variable
          | Binop Binop Expr Expr
          | Unop Unop Expr
          | Call Ident [Expr]
          deriving (Eq, Show)

-- statement
data Stmt = Expr Expr
          | Assign Ident Expr
          | Ret Expr
          | If Expr Block
          | While Expr Block
          | Def Ident [Ident] Block
          deriving (Eq, Show)

-- parametrized type from which the free monad StmtM is constructed
data StmtF next = StmtF Stmt next
    deriving (Eq, Show, Functor)

liftF = Free . fmap return . flip StmtF ()

-- helper functions for writing eDSL
expr e = liftF $ Expr e
assign lhs rhs = liftF $ Assign lhs rhs
ret e = liftF $ Ret e
if' e true = liftF $ If e true
while e block = liftF $ While e block
def name args block = liftF $ Def name args block

data StmtM a = Free (StmtF (StmtM a))
             | Pure a
             deriving (Eq, Show, Functor)

type Block = StmtM ()

instance Applicative StmtM where
    pure = Pure
    (<*>) = ap

instance Monad StmtM where
    Pure a >>= f = f a
    Free x >>= f = Free $ fmap (>>= f) x
