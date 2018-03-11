class Functor w => Comonad w where
    -- co-unit
    -- return the current value
    extract :: w a -> a

    -- cojoin
    -- all possible states
    duplicate :: w a -> w (w a)
    duplicate = extend id

    -- co-bind
    -- allState[state1[x], ...]
    -- extend = f . duplicate ->
    -- return all current states and apply f
    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate


(=>>) :: Comonad w => w b -> (w b -> a) -> w a
(=>>) = flip extend


liftW :: Comonad w => (a -> b) -> w a -> w b
liftW f = extend $ f . extract


instance Comonad ((,) e) where
  duplicate p = (fst p, p)
  extract = snd


type Env e a = (e, a)


ask :: Env e a -> e
ask = fst


local :: (e -> e') -> Env e a -> Env e' a
local f (e, a) = (f e, a)


initial :: (Int, Int)
initial = (n, n) where
  n = 0


experiment :: (Int, Int)
experiment = fmap (+10) initial


result :: Int
result = extract experiment


initialValue :: Int
initialValue = extract $ experiment =>> ask
