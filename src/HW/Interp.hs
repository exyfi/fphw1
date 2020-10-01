module HW.Interp where

import HW.Lang
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Text.Read (readEither)
import qualified Data.Map.Strict as M

-- Map: variable name -> variable value
type Scope = M.Map Ident Value

data Err = NotInScope Ident
         | TypeMismatch Binop Value Value
         | UTypeMismatch Unop Value
         | ArgumentMismatch Ident Int
         | EarlyReturn Value -- return from a function, we catch this error around function call
         | ValueError String
         deriving (Eq, Show)

-- interpreter monad, m is the real world
-- [Scope] is the call stack of scopes
type InterpT m = StateT [Scope] (ExceptT Err m)
type Interp = InterpT IO

data FakeWorld = FakeWorld { outputLines :: [String]
                           , inputLines :: [String]
                           }

type FakeIO = State FakeWorld

class Monad m => InterpImpl m where
    output :: String -> m ()
    input :: m String

instance InterpImpl IO where
    output = putStrLn
    input = getLine

instance InterpImpl FakeIO where
    output s = do
        state <- get
        let state' = state { outputLines = s : outputLines state }
        put state'

    input = do
        state <- get
        let (x:xs) = inputLines state
        let state' = state { inputLines = xs }
        put state'
        return x

-- get variable value, or throw NotInScope if the variable is not in scope
getValue :: InterpImpl m => Ident -> InterpT m Value
getValue "print" = return $ VBuiltin Print
getValue "input" = return $ VBuiltin Input
getValue "str" = return $ VBuiltin Str
getValue "int" = return $ VBuiltin Int
getValue ident = do
    scope <- get
    let value = msum $ (M.lookup ident) <$> scope
    case value of
      Just x  -> return x
      Nothing -> throwError $ NotInScope ident

-- sets variable value in the innermost scope
setValue :: InterpImpl m => Ident -> Value -> InterpT m ()
setValue ident value = modify $ \(x:xs) -> (M.insert ident value x):xs

-- execute a binary operation
binop :: InterpImpl m => Binop -> Value -> Value -> InterpT m Value
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
    eq <- binop Eq l r
    case eq of
      VBool beq -> return $ VBool $ not beq
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

-- execute a unary operation
unop :: InterpImpl m => Unop -> Value -> InterpT m Value
unop Not val = return $ VBool $ not $ isTruthy val
unop Neg (VInt x) = return $ VInt $ negate x
unop op expr = throwError $ UTypeMismatch op expr

-- str(value)
toString :: Value -> String
toString VNone = "None"
toString (VBool x) = show x
toString (VInt x) = show x
toString (VString x) = x

-- call builtin function with arguments
builtin :: InterpImpl m => Builtin -> [Value] -> InterpT m Value

builtin Print msg = do
    let s = concat $ map toString msg
    lift $ lift $ output s
    return VNone

builtin Input [] = do
    s <- lift $ lift input
    return $ VString s

builtin Str [v] = return . VString $ toString v

builtin Int [(VInt x)] = return $ VInt x
builtin Int [(VBool False)] = return $ VInt 0
builtin Int [(VBool True)] = return $ VInt 1
builtin Int [(VString s)] = case readEither s of
                              Left err -> throwError $ ValueError err
                              Right val -> return $ VInt val
builtin Int [x] = throwError . ValueError $ "Failed to parse int from " <> (toString x)

builtin fun args = throwError $ ArgumentMismatch (show fun) (length args)

eval :: InterpImpl m => Expr -> InterpT m Value
eval (Lit x) = return x
eval (Ref ident) = getValue ident
eval (Binop op lhs rhs) = do
    l <- eval lhs
    r <- eval rhs
    binop op l r
eval (Unop op expr) = do
    val <- eval expr
    unop op val
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

isTruthy :: Value -> Bool
isTruthy val = case val of
      VInt 0 -> False
      VString "" -> False
      VBool False -> False
      VNone -> False
      _ -> True

interp :: InterpImpl m => Block -> InterpT m ()
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
          val <- isTruthy <$> eval e
          when val $ interp true >> return ()
          interp next
      While e block -> do
          val <- isTruthy <$> eval e
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
             UTypeMismatch op expr -> putStrLn $ "Attempt to perform " <> (show op) <> " on " <> (show expr)
             ArgumentMismatch fun n -> putStrLn $ "Attempt to call " <> fun <> " with " <> (show n) <> " arguments"
             EarlyReturn _ -> putStrLn "Return from top level"
             ValueError msg -> putStrLn $ "ValueError: " <> msg
       Right () -> return ()

runInterpFake :: InterpT FakeIO () -> [String] -> Either Err [String]
runInterpFake i inp =
    case val of
        Left err -> Left err
        Right st -> Right $ outputLines state
    where (val, state) = runState (runExceptT $ evalStateT i [M.empty]) (FakeWorld { inputLines = inp, outputLines = [] })
