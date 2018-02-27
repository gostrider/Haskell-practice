import Prelude hiding ((.))

-- class Functor w => Comonad w where
--     extract :: w a -> a
--
--     duplicate :: w a -> w (w a)
--     duplicate = extend id
--
--     extend :: (w a -> b) -> w a -> w b
--     extend f = fmap f . duplicate
--
--
-- (=>>) :: Comonad w => w b -> (w b -> a) -> w a
-- (=>>) = flip extend
--
--
-- liftW :: Comonad w => (a -> b) -> w a -> w b
-- liftW f = extend $ f . extract
--
--
-- instance Comonad ((,) e) where
--   duplicate p = (fst p, p)
--   extract = snd


type Option = String


data Config =
  MkConfig [Option] deriving (Show)


configBuilder :: [Option] -> Config
configBuilder = MkConfig


defaultConfig :: [Option] -> Config
defaultConfig opts = MkConfig $ ["-Wall"] ++ opts


-- profile :: ([Option] -> Config) -> Config
-- profile builder = builder ["-prof", "-auto-all"]
--
--
-- goFaster :: ([Option] -> Config) -> Config
-- goFaster builder = builder ["-O2"]


extract :: ([Option] -> Config) -> Config
extract builder = builder []


-- simulate return self as return a function to accept next builder
profile'  :: ([Option] -> Config) -> ([Option] -> Config)
profile' builder =
    \options -> builder $ ["-prof", "-auto-all"] ++ options


goFaster' :: ([Option] -> Config) -> ([Option] -> Config)
goFaster' builder =
    \options -> builder $ ["-O2"] ++ options


(.) :: a -> (a -> b) -> b
x . f = f x

infixl 0 .

-- defaultConfig . profile' . goFaster' . extract
