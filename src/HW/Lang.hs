module HW.Lang where

import Control.Applicative
import Control.Monad

type Ident = String

data Builtin = Print
             | Input
             | Str
             | Int
             deriving (Eq, Show)

data Value = VInt Int
           | VBool Bool
           | VString String
           | VDef [Ident] Block
           | VBuiltin Builtin
           | VNone
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
           deriving (Eq, Show, Enum)

data Expr = Lit Value
          | Ref Ident
          | Binop Binop Expr Expr
          | Call Ident [Expr]
          deriving (Eq, Show)

data Stmt = Expr Expr
          | Assign Ident Expr
          | Ret Expr
          | If Expr Block
          | While Expr Block
          | Def Ident [Ident] Block
          deriving (Eq, Show)

data StmtF next = StmtF Stmt next
    deriving (Eq, Show, Functor)

liftF = Free . fmap return . flip StmtF ()

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

--example = do
--    def "max" ["a", "b"] $ do
--        ret (Binop Gt (Ref "a") (Ref "b"))

example = do
    assign "x" $ Call "input" []
    assign "y" $ Lit (VString "")
    assign "i" $ Lit (VInt 0)
    while (Binop Lt (Ref "i") (Lit (VInt 10))) $ do
        assign "y" $ Binop Add (Ref "x") (Ref "y")
        expr $ Call "print" [Ref "y"]
        assign "i" $ Binop Add (Ref "i") (Lit (VInt 1))
