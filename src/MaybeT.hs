module MaybeT where

import Control.Monad (liftM, ap)

{- MaybeT is isomorphic to runMaybeT
,    MaybeT :: m (MaybeT a) -> MaybeT m a
, runMaybeT :: MaybeT m a -> m (MaybeT a)
-}
newtype MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
}

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

{- maybe :: b -> (a -> b) -> Maybe a -> b
, 1 `maybe` (+1) (Just 1) == 2
, 1 `maybe` (+1) Nothing == 1
-}
altBindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
altBindMT x f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
bindMT x f = MaybeT $ do
    unwrapped <- runMaybeT x
    case unwrapped of
        Nothing -> return Nothing
        Just y -> runMaybeT (f y)

instance (Monad m) => Functor (MaybeT m) where
    fmap = liftM

instance (Monad m) => Applicative (MaybeT m) where
    pure = returnMT
    (<*>) = ap

instance (Monad m) => Monad (MaybeT m) where
    return = returnMT
    fail = failMT
    (>>=) = bindMT
