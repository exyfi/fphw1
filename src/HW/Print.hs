module HW.Print where

import HW.Lang
import Data.List

indent :: Int -> ShowS
indent x = showString $ replicate (4 * x) ' '

binopPrec :: Binop -> Int
binopPrec Mul = 6
binopPrec Div = 6
binopPrec Mod = 6
binopPrec Add = 5
binopPrec Sub = 5
binopPrec And = 3
binopPrec Or = 2
binopPrec _ = 3

binopSymb :: Binop -> String
binopSymb Add = "+"
binopSymb Sub = "-"
binopSymb Mul = "*"
binopSymb Div = "/"
binopSymb Mod = "%"
binopSymb Eq  = "=="
binopSymb Ne  = "!="
binopSymb Gt  = ">"
binopSymb Ge  = ">="
binopSymb Lt  = "<"
binopSymb Le  = "<="
binopSymb And = "and"
binopSymb Or  = "or"

showsExpr :: Int -> Expr -> ShowS
showsExpr _ (Lit (VInt i)) = shows i
showsExpr _ (Lit (VBool b)) = shows b
showsExpr _ (Lit (VString s)) = shows s
showsExpr _ (Ref var) = showString var
showsExpr d (Binop op lhs rhs) = showParen (d > b) $
    showsExpr (b + 1) lhs . showString (" " <> binopSymb op <> " ") . showsExpr (b + 1) rhs
    where b = binopPrec op
showsExpr _ (Call fun args) = showString fun . (showParen True $
    foldr (.) id (intersperse (showString ", ") $ map (showsExpr 0) args))

pretty :: Block -> String
pretty block = pretty 0 block ""
    where pretty _ (Pure ()) = id
          pretty i (Free (StmtF stmt next)) = indent i . f stmt . showChar '\n' . pretty i next
              where f (Expr expr) = showsExpr 0 expr
                    f (Assign ident expr) = showString ident . showString " = " . showsExpr 0 expr
                    f (Ret expr) = showString "return " . showsExpr 0 expr
                    f (If expr block) = showString "if " . showsExpr 0 expr . showString ":\n" . pretty (i + 1) block
                    f (While expr block) = showString "while " . showsExpr 0 expr . showString ":\n" . pretty (i + 1) block
                    f (Def name args block) = showString "def " . showString name . showString ("(" <> intercalate ", " args <> "):\n") . pretty (i + 1) block

edsl :: Block -> String
edsl block = "do\n" <> (pretty 1 block "")
    where pretty _ (Pure ()) = id
          pretty i (Free (StmtF stmt next)) = indent i . f stmt . showChar '\n' . pretty i next
              where f (Expr expr) = showString "expr $ " . shows expr
                    f (Assign ident expr) = showString "assign " . shows ident . showString " $ " . shows expr
                    f (Ret expr) = showString "ret $ " . shows expr
                    f (If expr block) = showString "if' (" . shows expr . showString ") $ do\n" . pretty (i + 1) block
                    f (While expr block) = showString "while (" . shows expr . showString ") $ do\n" . pretty (i + 1) block
                    f (Def name args block) = showString "def " . shows name . showChar ' ' . shows args . showString " $ do\n" . pretty (i + 1) block
