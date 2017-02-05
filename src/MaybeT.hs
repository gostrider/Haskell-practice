module MaybeT where

newtype MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
}

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

--failMT :: (Monad m) => t -> MaybeT m a
--failMT _ = MaybeT $ return Nothing

--altBindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
--altBindMT x f = MaybeT $ runMaybeT x >>= maybe (return Nothing) (runMaybeT . f)

--bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
--bindMT x f = MaybeT $ do
--    unwrapped <- runMaybeT x
--    case unwrapped of
--        Nothing -> return Nothing
--        Just y -> runMaybeT (f y)

--fmapMT :: (a -> b) -> MaybeT m a -> MaybeT m b
--apMT :: MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b

instance (Monad m) => Functor (MaybeT m) where
    fmap = undefined

instance (Monad m) => Applicative (MaybeT m) where
    pure = returnMT
    (<*>) = undefined

instance (Monad m) => Monad (MaybeT m) where
    {- maybe :: b -> (a -> b) -> Maybe a -> b
    , 1 `maybe` (+1) (Just 1) == 2
    , 1 `maybe` (+1) Nothing == 1
    -}
    return = returnMT
    fail _ = MaybeT $ return Nothing
    (>>=) x f = MaybeT $ runMaybeT x >>= (return Nothing) `maybe` (runMaybeT . f)
