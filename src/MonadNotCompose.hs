module MonadNotCompose where

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
  -}
  fmap f (Compose fgx) = Compose (fmap (fmap f) fgx)


instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a                      = Compose (pure (pure a))
  Compose fgf <*> Compose fgx = Compose ((<*>) <$> fgf <*> fgx)
