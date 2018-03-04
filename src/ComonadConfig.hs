type Option = String


data Config =
  MkConfig [Option] deriving (Show)


configBuilder :: [Option] -> Config
configBuilder = MkConfig


defaultConfig :: [Option] -> Config
defaultConfig opts = MkConfig $ "-Wall" : opts


profile :: ([Option] -> Config) -> Config
profile builder = builder ["-prof", "-auto-all"]


goFaster :: ([Option] -> Config) -> Config
goFaster builder = builder ["-O2"]


extract :: ([Option] -> Config) -> Config
extract builder = builder []


extend :: (([Option] -> Config) -> Config) -> ([Option] -> Config) -> ([Option] -> Config)
extend setter builder opts2 = setter (\opts1 -> builder (opts1 ++ opts2))


-- simulate return self as return a function to accept next builder
profile'  :: ([Option] -> Config) -> ([Option] -> Config)
profile' builder options = builder (["-prof", "-auto-all"] ++ options)


goFaster' :: ([Option] -> Config) -> ([Option] -> Config)
goFaster' builder options = builder ("-O2" : options)


(#) :: a -> (a -> b) -> b
x # f = f x

infixl 0 #


config0 :: Config
config0 =
  defaultConfig # profile' # goFaster' # extract

config1 :: Config
config1 =
  defaultConfig # \_b0 ->
    let _b1 =     extend (\this -> this # profile ) _b0
    in  extract $ extend (\this -> this # goFaster) _b1

-- equivalent to:
config2 :: Config
config2 =
  defaultConfig # \_b0 ->
    let _b1 = extend profile _b0
    in goFaster _b1

-- which reduces to:
config3 :: Config
config3 = goFaster $ extend profile defaultConfig
