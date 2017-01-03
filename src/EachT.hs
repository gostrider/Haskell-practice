module EachT where

import Control.Monad.Identity
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader

import Data.Maybe

type CustomType = Int
data CustomResult = ResVal CustomType deriving Show

base :: CustomResult
base = ResVal 1

{- Identity monad -}

type Identity' x = Identity x

runIdentity' :: Identity' a -> a
runIdentity' x = runIdentity x

transform :: Identity' CustomResult
transform = return base

{- Transform with ExceptT -}

type WithE a = ExceptT String Identity a

runExcept' :: WithE a -> Either String a
runExcept' m = runIdentity' $ runExceptT m

transExcept :: Maybe CustomResult -> WithE CustomResult
transExcept x = do
    case x of
        Nothing -> throwE "error"
        Just x -> return x

{- Transform with WriterT -}

type WithEW a = WriterT [String] (ExceptT String Identity) a

runWriter' :: WithEW a -> Either String (a, [String])
runWriter' m = (runExcept' . runWriterT) m

transWriter :: Maybe CustomResult -> WithEW CustomResult
transWriter x = do
    tell [show $ fromJust x]
    resb <- return $ transExcept x
    tell [show $ (runIdentity . runExceptT) resb]
    return $ ResVal 1

{- Transform with ReaderT -}

--type WithEWR a = ReaderT Int (WriterT [String] (ExceptT String Identity)) a
--
--runReader' :: Int -> WithEWR a -> Either String (a, [String])
--runReader' env m =  runWriter' $ runReaderT m env
--
--transReader :: WithEWR CustomResult
--transReader = do
--    readInput <- ask
--    return $ ResVal readInput