module EitherIO where

{-# LANGUAGE OverloadedStrings #-}

import Data.Text
import Data.Map as Map
import Control.Applicative (liftA2)
import qualified Data.Text.IO as T

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
                  deriving Show

data EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}

instance Functor (EitherIO e) where
    fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
    pure = EitherIO . return . Right
    (<*>) f x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
    return = pure
    (>>=) x f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)

--getToken :: EitherIO LoginError Text
--getToken = do
--    T.putStrLn "Enter email address"
--    input <- T.getLine
--    return (getDomain input)

getToken :: EitherIO LoginError Text
getToken = do
  EitherIO $ fmap Right (T.putStrLn "Enter email address:")
  input <- EitherIO (fmap Right T.getLine)
  EitherIO $ return (getDomain input)
