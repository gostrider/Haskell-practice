{-# LANGUAGE OverloadedStrings #-}

module EitherIO where

import Data.Text
import Data.Map as Map
import Control.Applicative (liftA2)
import qualified Data.Text.IO as T

data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
                  deriving Show

data ExceptIO e a = ExceptIO {
    runExceptIO :: IO (Either e a)
}

instance Functor (ExceptIO e) where
  fmap f = ExceptIO . fmap (fmap f) . runExceptIO

instance Applicative (ExceptIO e) where
  pure = ExceptIO . return . Right
  (<*>) f x = ExceptIO $ liftA2 (<*>) (runExceptIO f) (runExceptIO x)

instance Monad (ExceptIO e) where
  return = pure
  (>>=) x f = ExceptIO $ runExceptIO x >>= either (return . Left) (runExceptIO . f)

liftEither :: Either e a -> ExceptIO e a
liftEither = ExceptIO . return

liftIO :: IO a -> ExceptIO e a
liftIO = ExceptIO . fmap Right

throwE :: e -> ExceptIO e a
throwE = liftEither . Left

catchE :: ExceptIO e a -> (e -> ExceptIO e a) -> ExceptIO e a
catchE throwing handler =
  ExceptIO $ do
    result <- runExceptIO throwing
    case result of
      Left failure -> runExceptIO $ handler failure
      success -> return success

users :: Map Text Text
users = Map.fromList [ ("example.com", "qwerty123")
                     , ("localhost", "password")
                     ]

wrongPasswordHandler :: LoginError -> ExceptIO LoginError Text
wrongPasswordHandler WrongPassword = do
  liftIO $ T.putStrLn "Wrong Password, one more chance."
  userLogin
wrongPasswordHandler err = throwE err

--getToken :: ExceptIO LoginError Text
--getToken = do
--    T.putStrLn "Enter email address"
--    input <- T.getLine
--    return (getDomain input)

getDomain :: Text -> Either LoginError Text
getDomain email =
  case splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail

getToken :: ExceptIO LoginError Text
getToken = do
  ExceptIO $ fmap Right $ T.putStrLn "Enter email address:"
  input <- ExceptIO $ fmap Right T.getLine
  ExceptIO $ return $ getDomain input

userLogin :: ExceptIO LoginError Text
userLogin = do
  token    <- getToken
  userpwd  <- maybe (throwE NoSuchUser) return (Map.lookup token users)
  password <- liftIO $ T.putStrLn "Enter your password: " >> T.getLine

  if userpwd == password then return token else throwE WrongPassword

loginDialogue :: ExceptIO LoginError ()
loginDialogue = do
  let retry = userLogin `catchE` wrongPasswordHandler
  token <- retry `catchE` printError
  liftIO $ T.putStrLn $ append "Logged in with token: " token

parseResult :: Either LoginError Text -> Text
parseResult (Right token)        = append "Logged in with token: " token
parseResult (Left InvalidEmail)  = "Invalid email address entered."
parseResult (Left NoSuchUser)    = "No user with that email exists."
parseResult (Left WrongPassword) = "Wrong password."

parseError :: LoginError -> Text
parseError WrongPassword = "Wrong password, one more time."
parseError NoSuchUser    = "No user with that email exists."
parseError InvalidEmail  = "Invalid email address entered."

printResult :: Either LoginError Text -> IO ()
printResult = T.putStrLn . parseResult

printError :: LoginError -> ExceptIO LoginError a
printError err = do
  liftIO . T.putStrLn $ parseError err
  throwE err



