module HW.Interp where

import HW.Lang
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M

type Scope = M.Map Ident Value

data Err = NotInScope Ident
         | TypeMismatch Binop Value Value
         | ArgumentMismatch Ident Int
         | EarlyReturn Value -- return from a function, we catch this error around function call
         deriving (Eq, Show)

type Interp = StateT [Scope] (ExceptT Err IO)

getValue :: Ident -> Interp Value
getValue "print" = return $ VBuiltin Print
getValue "input" = return $ VBuiltin Input
getValue ident = do
    scope <- get
    let value = msum $ (M.lookup ident) <$> scope
    case value of
      Just x  -> return x
      Nothing -> throwError $ NotInScope ident

setValue :: Ident -> Value -> Interp ()
setValue ident value = modify $ \(x:xs) -> (M.insert ident value x):xs

binop :: Binop -> Value -> Value -> Interp Value
binop Add (VInt l) (VInt r) = return $ VInt $ l + r
binop Add (VString l) (VString r) = return $ VString $ l <> r
binop Sub (VInt l) (VInt r) = return $ VInt $ l - r
binop Mul (VInt l) (VInt r) = return $ VInt $ l * r
binop Div (VInt l) (VInt r) = return $ VInt $ l `div` r
binop Mod (VInt l) (VInt r) = return $ VInt $ l `mod` r
binop Eq (VInt l) (VInt r) = return $ VBool $ l == r
binop Eq (VBool l) (VBool r) = return $ VBool $ l == r
binop Eq (VString l) (VString r) = return $ VBool $ l == r
binop Eq VNone VNone = return $ VBool True
binop Eq _ _ = return $ VBool False
binop Ne l r = do
    (VBool eq) <- binop Eq l r
    return $ VBool $ not eq
binop Gt (VInt l) (VInt r) = return $ VBool $ l > r
binop Gt (VBool l) (VBool r) = return $ VBool $ l > r
binop Gt (VString l) (VString r) = return $ VBool $ l > r
binop Ge (VInt l) (VInt r) = return $ VBool $ l >= r
binop Ge (VBool l) (VBool r) = return $ VBool $ l >= r
binop Ge (VString l) (VString r) = return $ VBool $ l >= r
binop Lt (VInt l) (VInt r) = return $ VBool $ l < r
binop Lt (VBool l) (VBool r) = return $ VBool $ l < r
binop Lt (VString l) (VString r) = return $ VBool $ l < r
binop Le (VInt l) (VInt r) = return $ VBool $ l <= r
binop Le (VBool l) (VBool r) = return $ VBool $ l <= r
binop Le (VString l) (VString r) = return $ VBool $ l <= r
binop And (VBool l) (VBool r) = return $ VBool $ l && r
binop Or (VBool l) (VBool r) = return $ VBool $ l || r
binop op lhs rhs = throwError $ TypeMismatch op lhs rhs

builtin :: Builtin -> [Value] -> Interp Value
builtin Print msg = do
    lift $ lift $ print msg
    return VNone
builtin Input [] = do
    s <- lift $ lift getLine
    return $ VString s
builtin Input args = throwError $ ArgumentMismatch "input" (length args)

eval :: Expr -> Interp Value
eval (Lit x) = return x
eval (Ref ident) = getValue ident
eval (Binop op lhs rhs) = do
    l <- eval lhs
    r <- eval rhs
    binop op l r
eval (Call fun args) = do
    f <- getValue fun
    vals <- mapM eval args
    case f of
      VDef params block -> do
          when (length vals /= length params) $
              throwError $ ArgumentMismatch fun (length vals)
          modify (M.fromList (zip params vals):) -- push frame
          retval <- (interp block >> return VNone) `catchError` (\case
            EarlyReturn v -> return v
            err -> throwError err)
          modify tail -- pop frame
          return retval
      
      VBuiltin fun -> builtin fun vals

      _ -> throwError $ NotInScope fun

evalBool :: Expr -> Interp Bool
evalBool e = do
    val <- eval e
    return $ case val of
      VInt 0 -> False
      VString "" -> False
      VBool False -> False
      VNone -> False
      _ -> True

interp :: Block -> Interp ()
interp (Pure ()) = return ()
interp this@(Free (StmtF stmt next)) = do
    case stmt of
      Expr e -> do
          eval e
          interp next
      Assign lhs rhs -> do
          v <- eval rhs
          setValue lhs v
          interp next
      Ret e -> do
          v <- eval e
          throwError $ EarlyReturn v
      If e true -> do
          val <- evalBool e
          when val $ interp true >> return ()
          interp next
      While e block -> do
          val <- evalBool e
          if val then interp block >> interp this
                 else interp next
      Def fun params block -> do
          setValue fun $ VDef params block
          interp next

runInterp :: Interp () -> IO ()
runInterp i = do
     res <- runExceptT $ evalStateT i [M.empty]
     case res of
       Left err -> do
           putStrLn "=== Interpreter exception ==="
           case err of
             NotInScope i -> putStrLn $ "Identifier not found: " <> (show i)
             TypeMismatch op lhs rhs -> putStrLn $ "Attempt to perform " <> (show op) <> " on " <> (show lhs) <> " and " <> (show rhs)
             ArgumentMismatch fun n -> putStrLn $ "Attempt to call " <> fun <> " with " <> (show n) <> " arguments"
             EarlyReturn _ -> putStrLn "Return from top level"
       Right () -> return ()
