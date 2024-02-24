module Message (
  Message (..),
  InitRequest (..),
  InitResponse (..),
  testMsg,
  testJs,
) where

import Data.Aeson (FromJSON, ToJSON, encode, object, parseJSON, toJSON, withObject, (.:))
import Data.Aeson.Decoding (eitherDecode)
import Data.Aeson.Types ((.=))
import Data.Text (Text)
import GHC.Natural (Natural)

{-
{src: "c1",
 dest: "n1",
 body: {msg_id: 1,
        type: "init",
        node_id: "n1",
        node_ids: ["n1"]}}

{src: "n1",
 dest: "c1",
 body: {msg_id: 123
        in_reply_to: 1
        type: "init_ok"}}
-}

data Message a = Message
  { src :: Text
  , dst :: Text
  , body :: a
  }
  deriving stock (Show)

data InitRequest = InitRequest Natural Text Text [Text]
  deriving stock (Show)

data InitResponse = InitResponse Natural Natural Text
  deriving stock (Show)

-- * Serialization
instance ToJSON a => ToJSON (Message a) where
  toJSON (Message _src _dest _body) =
    object
      [ "src" .= _src
      , "dest" .= _dest
      , "body" .= _body
      ]

-- {src: \"c1\",dest: \"n1\",body: {msg_id: 1,type: \"init\",node_id: \"n1\",node_ids: [\"n1\"]}}
-- {src: \"n1\", dest: \"c1\", body: {msg_id: 123, in_reply_to: 1, type: \"init_ok\"}}

instance FromJSON a => FromJSON (Message a) where
  parseJSON = withObject "Message" $ \v ->
    Message
      <$> v .: "src"
      <*> v .: "dest"
      <*> v .: "body"

instance ToJSON InitRequest where
  toJSON (InitRequest msg_id msg_type node_id node_ids) =
    object
      [ "msg_id" .= msg_id
      , "type" .= msg_type
      , "node_id" .= node_id
      , "node_ids" .= node_ids
      ]

instance FromJSON InitRequest where
  parseJSON = withObject "InitRequest" $ \v ->
    InitRequest
      <$> v .: "msg_id"
      <*> v .: "type"
      <*> v .: "node_id"
      <*> v .: "node_ids"

instance ToJSON InitResponse where
  toJSON (InitResponse msg_id in_reply_to msg_type) =
    object
      [ "msg_id" .= msg_id
      , "in_reply_to" .= in_reply_to
      , "type" .= msg_type
      ]

instance FromJSON InitResponse where
  parseJSON = withObject "InitResponse" $ \v ->
    InitResponse
      <$> v .: "msg_id"
      <*> v .: "in_reply_to"
      <*> v .: "type"

testMsg :: Message InitResponse
testMsg =
  Message "from" "to" $
    InitResponse 1 2 "3"

testJs :: Either String (Message InitResponse)
testJs = eitherDecode $ encode testMsg :: Either String (Message InitResponse)
