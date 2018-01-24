module LensesLearn

-- Original form
-- get :: s -> a
-- set :: s -> a -> s


{-
Monomorphic types

get :: (a, a') -> a
get (x, y) = x


set :: (a, a') -> a -> (a, a')
set (x, y) x' = (x', y)

-}

-- Same function implementation

{-
Polymorphic types

get :: (a, a') -> a
get (x, y) = x

Work with (Int, Bool) & (String, Bool)

set :: (a, a') -> b -> (b, a')
set (x, y) x' = (x', y)

-}

{-
Laws

set -> get = set
get . set = id

get -> set = id
set s $ get s = s

set (set s a) b = set s b
-}

{-
Algebric Structure of Lens

"this" is OO
s -> (a, b -> t)
s -> Store a s

-}

data Store a s = Store a (a -> s)


-- as get
-- Functor : a -> F a
fstl :: (a, b) -> Store a (a, b)
fstl (x, y) = Store x (\x' -> (x', y))


instance Functor (Store a) where
  fmap f (Store x h) = Store x (f . h)


{-
Algebra

       Store a s -> s
alg :: f       s -> s


coAlgebra
         s -> Store a s
coalg :: s -> f       s
-}


class (Functor w) => Comonad w where
  extract   :: w a -> a
  duplicate :: w a -> w (w a)


instance Comonad (Stor a) where
  -- extract :: Store a s -> s
  extract (Store x h) = h x
  -- duplicate :: Store a s -> Store a (Store a s)
  duplicate (Store x h) = Store x (\y -> Store y h)


{-
Lens :: s -> Store a s

type Coalgebra w s = s -> w s

coalg :: Coalgebra w s
extract . coalg = id

   coalg
s -------> w s
  \        |
   \       |
id  \      | extract
     \     |
      \    |
       \   v
        \->s

fmap coalg . coalg = duplicate . coalg

            coalg
      s ------------> w s
coalg |               | duplicate
      v               v
      w s ----------> w (w s)
          fmap coalg

coalg s = Store (get s) (set s)

extract    . coalg = id <> set s $ get s = s
duplicate  . coalg      <> Store (get s) (\y -> Stroe y (set s))
fmap coalg . coalg      <> Store (get s) ((\s' -> Store (get s') (set s')) . set s)
-}
