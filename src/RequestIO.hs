{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module RequestIO (References (..), Payload (..), requestSample, parseJSON) where

import Data.Aeson
import Network.HTTP.Simple
import qualified Data.Yaml as Yaml
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8

data References = AvatarOnly  { avatarUrl :: String }
                | AvatarVoice { avatarUrl :: String, url :: String }
                | AvatarImage { avatarUrl :: String, url :: String, preview :: String }
                  deriving Show

data Payload =
    Payload { isGroup :: Bool
            , senderID :: String
            , recipientID :: [String]
            , messageType :: String
            , message :: String
            , references :: References
            } deriving Show

data SendMessageResp =
    SendMessageResp { resp :: [EachResp]
                    } deriving Show

data EachResp =
    EachResp { statusCode :: Integer
              , msgID :: String
              , xDelayTime :: String
              , recipientUserID :: String
              } deriving Show

instance ToJSON Payload where
    toJSON p =
        object [ "is_group" .= isGroup p
               , "sender_user_id" .= senderID p
               , "recipient_user_id" .= recipientID p
               , "message_type" .= messageType p
               , "message" .= message p
               , "references" .= (multiReferences . references) p ]

instance FromJSON SendMessageResp where
    parseJSON (Object v) =
        SendMessageResp <$> v .: "resp"

instance FromJSON EachResp where
    parseJSON (Object v) =
            EachResp <$> v .: "status_code"
                     <*> v .: "msg_id"
                     <*> v .: "x_delay_time"
                     <*> v .: "recipient_user_id"

multiReferences :: References -> Value
multiReferences AvatarOnly {avatarUrl = avatarUrl} =
    object [ "avatar_url" .= avatarUrl ]
multiReferences AvatarVoice {avatarUrl = avatarUrl, url = url} =
    object [ "avatar_url" .= avatarUrl
           , "url" .= url ]
multiReferences AvatarImage {avatarUrl = avatarUrl, url = url, preview = preview} =
    object [ "avatar_url" .= avatarUrl
           , "url" .= url
           , "preview" .= preview ]

payload :: Payload
payload =
    Payload
        False
        "06be5f70-c8f7-414d-80cd-5e4984d41d81" ["5b587d76-54aa-455d-bca3-77ae8afddaa6"]
        "text" "hello"
        (AvatarOnly "http://d2h8t8into6exa.cloudfront.net/user_comic_avatars/VkKDIkBHsd.jpg")

requestSample :: IO ()
requestSample = do
    let request = setRequestBodyJSON payload $ "POST http://staging-chat.chaatz.com:4000/v1/chat/_send_message"
    response <- httpJSON request
    putStrLn $ show $ getResponseStatusCode response
    print $ getResponseHeader "Content-Type" response
--    L8.putStrLn $ getResponseBody response
    -- For parsing JSON
--    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
    print $ show (getResponseBody response :: SendMessageResp)
