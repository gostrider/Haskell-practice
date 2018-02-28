Reader r a = Reader { runReader :: r -> a }


Reader    :: (r -> a) -> Reader r a
runReader :: Reader r a -> r -> a


runReader :: Reader Person String -> Person -> String
runReader report :: Person -> String
runReader report fred :: String


-- runReader ask age         fred # 1
-- runReader (Reader id) age fred # 1

-- runReader (asks age)   fred # 1
-- runReader (Reader age) fred # 1


-- runReader (fmap id Reader age) fred
-- runReader (fmap id $ asks age) fred
-- runReader (local id ask) age fred


-- fmap f mr = Reader $ \r -> f  $ runReader mr         r
-- runReader  (Reader $ \r -> id $ runReader (asks age) r) fred
-- r = fred


-- runReader (Reader (+1)) 1 # 2
-- runReader ((+1) <$> (Reader (+1))) 1 # 3
-- runReader (pure (+1) <*> (Reader (+1))) 1 # 3
-- runReader (pure (*) <*> (Reader (+2) <*> (Reader (+1))) 3 # 20
-- (x + 2) * (x + 1)

-- runReader ( local (+1) (Reader (+1)) ) 1 # 3
-- runReader (asks (+1)) 1 # 2
-- runReader ask (+1) 1 # 2


-- Reader r m | m -> r
-- ask :: m r
-- local :: (r -> r) -> m a -> m a
-- asks | reader :: (r -> a) -> m a


-- runReader (asks age >>= return . (+1)) fred
