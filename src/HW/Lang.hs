module HW.Lang where

import Control.Applicative
import Control.Monad

type Ident = String

data Value = VInt Int
           | VBool Bool
           | VString String
           deriving (Eq, Show)

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
           | Not
           deriving (Eq, Show, Enum)

data Expr = Lit Value
          | Ref Ident
          | Binop Binop Expr Expr
          deriving (Eq, Show)

data Stmt = Expr Expr
          | Assign Ident Expr
          | If Expr Block
          | IfElse Expr Block Block
          | While Expr Block
          | Def Ident [Ident] Block
          | Ret Expr
          deriving (Eq, Show)

data StmtF next = StmtF Stmt next
    deriving (Eq, Show, Functor)

liftF = Free . fmap return . flip StmtF ()

expr e = liftF $ Expr e
assign lhs rhs = liftF $ Assign lhs rhs
if' e true = liftF $ If e true
ifelse e true false = liftF $ IfElse e true false
while e block = liftF $ While e block
def name args block = liftF $ Def name args block
ret e = liftF $ Ret e

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

example = do
    def "max" ["a", "b"] $ do
        ret (Binop Gt (Ref "a") (Ref "b"))
