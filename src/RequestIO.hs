{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RequestIO (References (..), Payload (..), requestSample) where

import Data.Aeson
import Network.HTTP.Simple
import Data.Text (Text(..))
import Control.Monad (liftM)
import Data.Time.Clock (UTCTime)
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Time.Format (parseTimeM, defaultTimeLocale)

data References = AvatarOnly  { avatarUrl :: String }
                | AvatarVoice { avatarUrl :: String, url :: String }
                | AvatarImage { avatarUrl :: String, url :: String, preview :: String }
                  deriving Show

data Payload = Payload { isGroup     :: Bool
                       , senderID    :: String
                       , recipientID :: [String]
                       , messageType :: String
                       , message     :: String
                       , references  :: References
                       } deriving Show

data MessageID = Fail Bool
               | Succ Text
                 deriving Show

data RequestResp = RequestResp { resp :: [SendMessageResp]
                               } deriving Show

data SendMessageResp = SendMessageResp { statusCode      :: Int
                                       , msgID           :: MessageID
                                       , xDelayTime      :: Maybe UTCTime
                                       , recipientUserID :: String
                                       } deriving Show

instance ToJSON Payload where
    toJSON p = object [ "is_group"          .= isGroup p
                      , "sender_user_id"    .= senderID p
                      , "recipient_user_id" .= recipientID p
                      , "message_type"      .= messageType p
                      , "message"           .= message p
                      , "references"        .= (multiReferences . references) p ]

instance FromJSON RequestResp where
    parseJSON (Object v) = RequestResp <$> v .: "resp"

instance FromJSON MessageID where
    parseJSON (String s) = return $ Succ s
    parseJSON (Bool b)   = return $ Fail b
    parseJSON _          = fail "failed to parse msg_id"

instance FromJSON SendMessageResp where
    parseJSON (Object v) = SendMessageResp <$> v .: "status_code"
                                           <*> v .: "msg_id"
                                           <*> (liftM parseUTCTime $ v .: "x_delay_time")
                                           <*> v .: "recipient_user_id"

    parseJSON _ = fail "Invalid types"

multiReferences :: References -> Value
multiReferences AvatarOnly {avatarUrl = avatarUrl} =
    object [ "avatar_url" .= avatarUrl ]

multiReferences AvatarVoice {avatarUrl = avatarUrl, url = url} =
    object [ "avatar_url" .= avatarUrl
           , "url"        .= url ]

multiReferences AvatarImage {avatarUrl = avatarUrl, url = url, preview = preview} =
    object [ "avatar_url" .= avatarUrl
           , "url"        .= url
           , "preview"    .= preview ]

{-  Format UTC time
    myTime <- getCurrentTime
    (formatTime defaultTimeLocale "%Y%m%eT%H:%M:%S") myTime
-}

parseUTCTime :: String -> Maybe UTCTime
parseUTCTime = parseTimeM False defaultTimeLocale "%Y%m%eT%H:%M:%S"

payload :: Payload
payload =
    Payload
        False
        "06be5f70-c8f7-414d-80cd-5e4984d41d81" [ "1b587d76-54aa-455d-bca3-77ae8afddaa6"
                                               , "5b587d76-54aa-455d-bca3-77ae8afddaa6" ]
        "text" "hello"
        (AvatarOnly "http://d2h8t8into6exa.cloudfront.net/user_comic_avatars/VkKDIkBHsd.jpg")

requestSample :: IO ()
requestSample = do
    let request = setRequestBodyJSON payload "POST http://staging-chat.chaatz.com:4000/v1/chat/_send_message"
    response <- httpJSON request
    print $ show (getResponseBody response :: RequestResp)
    putStrLn $ show $ getResponseStatusCode response
    print $ getResponseHeader "Content-Type" response
    {-
    L8.putStrLn $ getResponseBody response
--     For parsing JSON
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
    -}
