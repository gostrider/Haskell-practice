module EachT where

import Control.Monad.Identity
import Control.Monad.Trans.Writer

{- Identity monad -}

type Identity' x = Identity x

runIdentity' :: Identity' a -> a
runIdentity' x = runIdentity x

transform :: Identity' Int
transform = return 1

{- Transform writerT -}
