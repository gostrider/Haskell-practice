{-# LANGUAGE MultiParamTypeClasses #-}

module MonadNotCompose where

import Control.Monad (join)
import Data.Traversable (Traversable, sequence)


{-
   Try to validate 2 Generic types,
   like, List [] or Maybe
   runCompose Similar to runReader -> Reader :: isomorphic

   Compose has kind of (* -> *) -> (* -> *) -> * -> *
-}
newtype Compose f g x = Compose { runCompose :: f (g x) }

-- Compose f g x is a Type Constructor
instance (Functor f, Functor g) => Functor (Compose f g) where
  {-
     Something like applicative function f (a -> b)
     lift (a -> b) -> f (a -> b), then apply f to fgx

     Compose fgx is Data Constructor, which is runCompose

     runCompose $ fmap (+1) (Compose [Just 1])
     runCompose $ Compose fmap (fmap (+1)) [Just 1]
     runCompose $ Compose fmap (\x -> fmap (+1) x) [Just 1]
     runCompose $ Compose fmap (\x -> fmap (\y -> y + 1) (Just 1)) [Just 1]

     #output [Just 1] -> [Just 2] or [[1]] -> [[2]]
  -}
  fmap f (Compose fgx) = Compose $ fmap (fmap f) fgx


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose $ pure $ pure a
  {-
    runCompose $ Compose [[(+1)]] <*> Compose [[1]]
    runCompose $ fmap (<*>) [[(+1)]] <*> [[1]]
    #output [Just 2] or [[2]]
  -}
  Compose fgf <*> Compose fgx = Compose $ fmap (<*>) fgf <*> fgx
  --                          = Compose $ (<*>) <$>  fgf <*> fgx


{-
  (<*>) :: Applicative f => f (s -> t) -> f s -> f t
  (>>=) :: Monad       m => m a -> (a -> m b) -> m b

  In (>>=), m b is determined by a;
  but in (<*>) does not.
-}

-- mif (return True) (return True) (return False)
-- mif (pure True) (pure True) (error "bad")
mif :: Monad m => m Bool -> m x -> m x -> m x
mif mbool mtrue mfalse = do
  b <- mbool
  if b then mtrue else mfalse


-- aif (pure True) (pure True) (error "bad")
-- Failed!
aif :: Applicative a => a Bool -> a x -> a x -> a x
aif abool atrue afalse = 
  let cond b t f = if b then t else f
  in pure cond <*> abool <*> atrue <*> afalse


class Commute f g where
  commute :: g (f x) -> f (g x)


-- instance (Monad f, Monad g, Commute f g) => Monad (Compose f g)

-- Monad Associativity Law :: m >>= f >>= g == m >>= (\x -> f x >>= g)

(>>>=) :: (Monad f, Monad g, Traversable g) => f (g a) -> (a -> f (g b)) -> f (g b)
m >>>= k = do
  a <- m
  b <- sequence $ fmap k a
  return $ join b


monad_assoc :: IO ()
monad_assoc = do
  unused <- getLine
  ans    <- getLine
  putStrLn ans

