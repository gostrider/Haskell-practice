module MTrans where

import Control.Monad.Identity
import Control.Monad.State (MonadState (..), get, put)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State (StateT, runStateT)
import Control.Monad.Trans.Writer

import Data.Maybe
import qualified Data.Map as Map

type Name = String

data Exp = Lit Integer   -- constants
         | Var Name      -- variables
         | Plus Exp Exp  -- addition
         | Abs Name Exp  -- lambda expression
         | App Exp Exp   -- function application
         deriving Show

data Value = IntVal Integer
           | FunVal Env Name Exp  -- closuers
           deriving Show

type Env = Map.Map Name Value

-- 12 + (id (4 + 2))
exampleExpr :: Exp
exampleExpr = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

{- Plain data type -}

eval0 :: Env -> Exp -> Value
eval0 _ (Lit i) = IntVal i
eval0 env (Var n) = fromJust $ Map.lookup n env
eval0 env (Plus e1 e2) = let IntVal i1 = eval0 env e1
                             IntVal i2 = eval0 env e2
                         in  IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) = let val1 = eval0 env e1
                            val2 = eval0 env e2
                        in
                            case val1 of
                                FunVal env' n body ->
                                    eval0 (Map.insert n val2 env') body

{- Add Identiy monad -}

type Eval1 a = Identity a

runEval1 :: Eval1 a -> a
runEval1 ev = runIdentity ev

--eval1 :: Monad m => Env -> Exp -> m Value
eval1 :: Env -> Exp -> Eval1 Value
eval1 env (Lit i) = return $ IntVal i
eval1 env (Var n) = return $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2) = do IntVal i1 <- eval1 env e1
                            IntVal i2 <- eval1 env e2
                            return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do val1 <- eval1 env e1
                           val2 <- eval1 env e2
                           case val1 of
                               FunVal env' n body ->
                                   eval1 (Map.insert n val2 env') body

{- Add ExceptT with IdentityT -}

example2a :: Exp
example2a = Plus (Lit 1) (Abs "x" (Var "x"))

example2b :: Exp
example2b = Var "x"


type Eval2 a = ExceptT String Identity a

runEval2 :: Eval2 a -> Either String a
runEval2 ev = runIdentity (runExceptT ev)

eval2a :: Env -> Exp -> Eval2 Value
eval2a env (Lit i) = return $ IntVal i
eval2a env (Var n) = return $ fromJust (Map.lookup n env)
eval2a env (Plus e1 e2) = do IntVal i1 <- eval2a env e1
                             IntVal i2 <- eval2a env e2
                             return $ IntVal (i1 + i2)
eval2a env (Abs n e) = return $ FunVal env n e
eval2a env (App e1 e2) = do val1 <- eval2a env e1
                            val2 <- eval2a env e2
                            case val1 of
                                FunVal env' n body ->
                                    eval2a (Map.insert n val2 env') body

eval2b :: Env -> Exp -> Eval2 Value
eval2b env (Lit i) = return $ IntVal i
eval2b env (Var n) = case Map.lookup n env of
                         Nothing -> throwE "Key Not Found"
                         Just x -> return x
eval2b env (Plus e1 e2) = do e1' <- eval2b env e1
                             e2' <- eval2b env e2
                             case (e1', e2') of
                                 (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                 _ -> throwE "type error"
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do val1 <- eval2b env e1
                            val2 <- eval2b env e2
                            case val1 of
                                FunVal env' n body ->
                                    eval2b (Map.insert n val2 env') body
                                _ -> throwE "type error"

eval2c :: Env -> Exp -> Eval2 Value
eval2c env (Lit i) = return $ IntVal i
eval2c env (Var n) = case Map.lookup n env of
                         Nothing -> throwE "Key Not Found"
                         Just x -> return x
eval2c env (Plus e1 e2) = do IntVal i1 <- eval2c env e1
                             IntVal i2 <- eval2c env e2
                             return $ IntVal (i1 + i2)
eval2c env (Abs n e) = return $ FunVal env n e
eval2c env (App e1 e2) = do FunVal env' n body <- eval2c env e1
                            val2               <- eval2c env e2
                            eval2c (Map.insert n val2 env') body

eval2 :: Env -> Exp -> Eval2 Value
eval2 env (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
                        Nothing -> throwE $ "unbound variable: " ++ n
                        Just x -> return x
eval2 env (Plus e1 e2) = do e1' <- eval2 env e1
                            e2' <- eval2 env e2
                            case (e1', e2') of
                                (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                                _ -> throwE "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do val1 <- eval2 env e1
                           val2 <- eval2 env e2
                           case val1 of
                               FunVal env' n body ->
                                   eval2 (Map.insert n val2 env') body
                               _ -> throwE "type error in application"

{- Add ReaderT -}

class MonadTrans t where
    lift :: Monad m => m a -> t m a

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ \_ -> m

type Eval3 a = ReaderT Env (ExceptT String Identity) a

-- runEval3 Map.empty $ eval3 exampleExpr
runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity $ runExceptT $ runReaderT ev env

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do env <- ask
                   case Map.lookup n env of
                       -- lifting ExceptT -> ReaderT ?
                       Nothing -> ReaderT $ \_ -> throwE $ "unbound variable: " ++ n
                       Just x -> return x
eval3 (Plus e1 e2) = do e1' <- eval3 e1
                        e2' <- eval3 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _ -> ReaderT $ \_ -> throwE "type error in addition"
eval3 (Abs n e) = do env <- ask; return $ FunVal env n e
eval3 (App e1 e2) = do val1 <- eval3 e1
                       val2 <- eval3 e2
                       case val1 of
                           FunVal env' n body ->
                               local (const (Map.insert n val2 env')) (eval3 body)
                           _ -> ReaderT $ \_ -> throwE "type error in application"

{- Add StateT -}

type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

-- runEval4 Map.empty 0 $ eval4 exampleExpr
runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity $ runStateT (runExceptT (runReaderT ev env)) st

tick :: (Num s, MonadState s m) => m ()
tick = do get >>= \x -> put (x + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do tick; return $ IntVal i
eval4 (Var n) = do tick
                   env <- ask
                   case Map.lookup n env of
                        Nothing -> ReaderT $ \_ -> throwE $ "unbound variable: " ++ n
                        Just x -> return x
eval4 (Plus e1 e2) = do tick
                        e1' <- eval4 e1
                        e2' <- eval4 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) ->
                                return $ IntVal (i1 + i2)
                            _ -> ReaderT $ \_ -> throwE "type error in addition"
eval4 (Abs n e) = do tick; ask >>= \env -> return $ FunVal env n e
eval4 (App e1 e2) = do tick
                       val1 <- eval4 e1
                       val2 <- eval4 e2
                       case val1 of
                            FunVal env' n body ->
                                local (const (Map.insert n val2 env')) (eval4 body)
                            _ -> ReaderT $ \_ -> throwE "type error in application"

{- Add WriterT -}

type Eval5 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) a

runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev = runIdentity $ runStateT (runWriterT $ runExceptT $ runReaderT ev env) st

eval5 :: Exp -> Eval5 Value
eval5 (Lit i) = do tick; return $ IntVal i
eval5 (Var n) = do tick
                   env <- ask
--                   tell [show n]
                   case Map.lookup n env of
                       Nothing -> ReaderT $ \_ -> throwE $ "unbound variable: " ++ n
                       Just x -> return x
eval5 (Plus e1 e2) = do tick
                        e1' <- eval5 e1
                        e2' <- eval5 e2
                        case (e1', e2') of
                            (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
                            _ -> ReaderT $ \_ -> throwE $ "type error in addition"
eval5 (Abs n e) = do tick; ask >>= \env -> return $ FunVal env n e
eval5 (App e1 e2) = do tick
                       val1 <- eval5 e1
                       val2 <- eval5 e2
                       case val1 of
                           FunVal env' n body ->
                            local (const $ Map.insert n val2 env') (eval5 body)
                           _ -> ReaderT $ \_ -> throwE "type error in application"