module ReaderM where

import Control.Monad.Reader hiding (Reader (..), ask, asks, runReader)

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap = liftM

instance Applicative (Reader r) where
    pure = return
    (<*>) = ap

instance Monad (Reader r) where
    return a = Reader $ \_ -> a
    m >>= k = Reader $ \r -> runReader (k (runReader m r)) r

data MyContext = MyContext
    { foo :: String
    , bar :: Int
    } deriving Show

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks f = Reader f

hello :: Reader String String
hello = do
    name <- ask
    return ("hello, " ++ name ++ "!")

bye :: Reader String String
bye = do
    name <- ask
    return ("bye, " ++ name ++ "!")

convo :: Reader String String
convo = do
    c1 <- hello
    c2 <- bye
    return $ c1 ++ c2

convo2 :: Reader String String
convo2 = hello >>= \c1 -> bye >>= \c2 -> return $ c1 ++ c2

computation :: Reader MyContext (Maybe String)
computation = do
    n <- asks bar
    x <- asks foo
    if n > 0
    then return $ Just x
    else return Nothing

computation2 :: Reader MyContext (Maybe String)
computation2 =
    asks bar >>= \n ->
        asks foo >>= \x ->
            if n > 0
            then return $ Just x
            else return Nothing

ex1 :: Maybe String
ex1 = runReader computation $ MyContext "aaa" 1

ex2 :: Maybe String
ex2 = runReader computation $ MyContext "bbb" 0
