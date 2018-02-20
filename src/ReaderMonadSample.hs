import Text.Printf (printf)

data Reader r a = Reader { runReader :: r -> a }


instance Functor (Reader r) where
  fmap f (Reader g) = Reader $ f . g


instance Applicative (Reader r) where
  pure    = Reader . const
  f <*> x = Reader $ \r -> (runReader f r) (runReader x r)


instance Monad (Reader r) where
  return         = Reader . const
  Reader g >>= f = Reader $ \r -> runReader (f $ g r) r


ask :: Reader a a
ask = Reader id


asks :: (r -> a) -> Reader r a
asks = Reader
-- asks f = Reader f
-- OR:
-- asks f = ask >>= \x -> return $ f x


local :: (r -> r) -> Reader r a -> Reader r a
local f g = Reader $ \e -> runReader g $ f e


data Person =
  Person { name :: String, age :: Int, email :: String }


ageInNYears :: Int -> Reader Person Int
ageInNYears n = asks age >>= \age' -> return $ age' + n


formatEmail :: Reader Person String
formatEmail = do
  name'  <- asks name
  email' <- asks email
  return $ printf "\"%s\" <%s>" name' email'


fred :: Person
fred = Person "fred" 1 "fred@example.com"


report :: Reader Person String
report = do
  five   <- ageInNYears 5
  mailto <- formatEmail
  return $ printf "Age in 5:\t%d\nEmail:\t\t%s" five mailto


main :: IO ()
main = putStrLn $ runReader report fred
