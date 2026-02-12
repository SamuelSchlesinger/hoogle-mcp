-- | Client for the Hoogle JSON API.
--
-- Calls the public Hoogle search endpoint with @mode=json&format=text@
-- to get plain-text results suitable for display to language models.
module HoogleMCP.Hoogle
    ( HoogleResult (..)
    , HoogleRef (..)
    , searchHoogle
    ) where

import Data.Aeson
import Data.ByteString.Char8 qualified as BS
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Network.HTTP.Simple

-- | A reference to a Hackage package or module.
data HoogleRef = HoogleRef
    { refUrl  :: !Text
    , refName :: !Text
    } deriving stock (Show)

instance FromJSON HoogleRef where
    parseJSON = withObject "HoogleRef" $ \v ->
        HoogleRef
            <$> v .: "url"
            <*> v .: "name"

-- | A single search result from the Hoogle API.
data HoogleResult = HoogleResult
    { resultUrl     :: !Text
    , resultModule  :: !(Maybe HoogleRef)
    , resultPackage :: !(Maybe HoogleRef)
    , resultItem    :: !Text
    , resultDocs    :: !Text
    } deriving stock (Show)

instance FromJSON HoogleResult where
    parseJSON = withObject "HoogleResult" $ \v ->
        HoogleResult
            <$> v .: "url"
            <*> v .:? "module"
            <*> v .:? "package"
            <*> v .: "item"
            <*> v .:? "docs" .!= ""

-- | Search the Hoogle API.
searchHoogle
    :: String  -- ^ Base URL (e.g. @https:\/\/hoogle.haskell.org@)
    -> Text    -- ^ Search query
    -> Int     -- ^ Maximum number of results
    -> IO [HoogleResult]
searchHoogle baseUrl query count = do
    baseReq <- parseRequest baseUrl
    let req = setRequestQueryString
            [ ("mode",   Just "json")
            , ("format", Just "text")
            , ("hoogle", Just (T.encodeUtf8 query))
            , ("start",  Just "1")
            , ("count",  Just (BS.pack (show count)))
            ] baseReq
    getResponseBody <$> httpJSON req
