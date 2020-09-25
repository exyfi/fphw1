module HW.Print where

import HW.Lang
import Data.List

indent :: Int -> ShowS
indent x = showString $ replicate (4 * x) ' '

pretty :: Int -> Block -> ShowS
pretty _ (Pure ()) = id
pretty i (Free (StmtF stmt next)) = indent i .f stmt . showChar '\n' . pretty i next
    where f (Expr expr) = shows expr
          f (Assign ident expr) = showString ident . showString " = " . shows expr
          f (Ret expr) = showString "return " . shows expr
          f (If expr block) = showString "if " . shows expr . showString ":\n" . pretty (i + 1) block
          f (IfElse expr true false) = showString "if " . shows expr . showString ":\n". pretty (i + 1) true . indent i . showString "else:\n" . pretty (i + 1) false
          f (While expr block) = showString "while " . shows expr . showString ":\n" . pretty (i + 1) block
          f (Def name args block) = showString "def " . showString name . showString ("(" <> intercalate ", " args <> "):\n") . pretty (i + 1) block
