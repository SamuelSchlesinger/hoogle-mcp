-- | MCP server for Hoogle.
--
-- Implements the Model Context Protocol over JSON-RPC 2.0 on stdio,
-- proxying search requests to the Hoogle JSON API.
module HoogleMCP.Server (run) where

import Control.Exception (SomeException, catch, displayException)
import Control.Monad (unless)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.IO

import HoogleMCP.Hoogle

--------------------------------------------------------------------------------
-- JSON-RPC types
--------------------------------------------------------------------------------

data Request = Request
    { reqId     :: !(Maybe Value)
    , reqMethod :: !Text
    , reqParams :: !(Maybe Value)
    }

instance FromJSON Request where
    parseJSON = withObject "Request" $ \v ->
        Request
            <$> v .:? "id"
            <*> v .: "method"
            <*> v .:? "params"

data Response = Response
    { respId   :: !(Maybe Value)
    , respBody :: !ResponseBody
    }

data ResponseBody
    = Result !Value
    | RpcError !Int !Text

instance ToJSON Response where
    toJSON Response {respId, respBody} =
        object $
            [ "jsonrpc" .= ("2.0" :: Text)
            , "id"      .= respId
            ] <> case respBody of
                Result v ->
                    ["result" .= v]
                RpcError code msg ->
                    ["error" .= object ["code" .= code, "message" .= msg]]

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

-- | Run the MCP server.
run :: IO ()
run = do
    hSetBuffering stdin  LineBuffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr LineBuffering
    baseUrl <- maybe "https://hoogle.haskell.org" id <$> lookupEnv "HOOGLE_URL"
    hPutStrLn stderr $ "hoogle-mcp: using " <> baseUrl
    loop baseUrl

loop :: String -> IO ()
loop baseUrl = do
    eof <- hIsEOF stdin
    unless eof $ do
        line <- BS.hGetLine stdin
        unless (BS.null line) $ do
            mResp <- handleLine baseUrl line
            case mResp of
                Nothing   -> pure ()
                Just resp -> do
                    LBS.hPut stdout (encode resp <> "\n")
                    hFlush stdout
        loop baseUrl

handleLine :: String -> BS.ByteString -> IO (Maybe Response)
handleLine baseUrl line =
    case eitherDecode (LBS.fromStrict line) of
        Left err -> pure $ Just $ Response Nothing (RpcError (-32700) (T.pack err))
        Right req ->
            dispatch baseUrl req
                `catch` \(e :: SomeException) ->
                    pure $ Just $ Response (reqId req) $
                        RpcError (-32603) (T.pack (displayException e))

--------------------------------------------------------------------------------
-- Dispatch
--------------------------------------------------------------------------------

dispatch :: String -> Request -> IO (Maybe Response)
dispatch baseUrl req = case reqMethod req of
    "initialize"                -> pure $ Just $ respond req initializeResult
    "initialized"               -> pure Nothing
    "notifications/initialized" -> pure Nothing
    "ping"                      -> pure $ Just $ respond req (object [])
    "tools/list"                -> pure $ Just $ respond req toolsListResult
    "tools/call"                -> Just <$> handleToolCall baseUrl req
    method                      -> pure $ Just $ Response (reqId req) $
        RpcError (-32601) ("Unknown method: " <> method)

respond :: Request -> Value -> Response
respond req result = Response (reqId req) (Result result)

--------------------------------------------------------------------------------
-- Initialize
--------------------------------------------------------------------------------

initializeResult :: Value
initializeResult = object
    [ "protocolVersion" .= ("2024-11-05" :: Text)
    , "capabilities"    .= object ["tools" .= object []]
    , "serverInfo"      .= object
        [ "name"    .= ("hoogle-mcp" :: Text)
        , "version" .= ("0.1.0" :: Text)
        ]
    ]

--------------------------------------------------------------------------------
-- Tools list
--------------------------------------------------------------------------------

toolsListResult :: Value
toolsListResult = object ["tools" .= tools]
  where
    tools :: [Value]
    tools =
        [ mkTool "hoogle_search"
            "Search Haskell libraries by function name or type signature.\n\
            \\n\
            \Query syntax:\n\
            \  - Function name: map, foldr, zip\n\
            \  - Type signature: a -> b, [a] -> Int\n\
            \  - Qualified name: Data.List.map\n\
            \  - Package filter: +base, -containers\n\
            \  - Combined: map +base"
            (object
                [ "type" .= ("object" :: Text)
                , "properties" .= object
                    [ "query" .= object
                        [ "type"        .= ("string" :: Text)
                        , "description" .= ("Search query" :: Text)
                        ]
                    , "count" .= object
                        [ "type"        .= ("integer" :: Text)
                        , "description" .= ("Maximum results (default: 10, max: 100)" :: Text)
                        ]
                    ]
                , "required" .= [("query" :: Text)]
                ])
        , mkTool "hoogle_info"
            "Get detailed information about a Haskell function or type.\n\
            \\n\
            \Returns the type signature, package/module, documentation, and\n\
            \Hackage link. For best results use a qualified name like Data.List.map."
            (object
                [ "type" .= ("object" :: Text)
                , "properties" .= object
                    [ "query" .= object
                        [ "type"        .= ("string" :: Text)
                        , "description" .= ("Function or type name" :: Text)
                        ]
                    ]
                , "required" .= [("query" :: Text)]
                ])
        ]

    mkTool :: Text -> Text -> Value -> Value
    mkTool name desc schema = object
        [ "name"        .= name
        , "description" .= desc
        , "inputSchema" .= schema
        ]

--------------------------------------------------------------------------------
-- Tool calls
--------------------------------------------------------------------------------

handleToolCall :: String -> Request -> IO Response
handleToolCall baseUrl req =
    case reqParams req of
        Just (Object obj)
            | Just (String name) <- KM.lookup "name" obj ->
                let args = case KM.lookup "arguments" obj of
                        Just (Object a) -> a
                        _               -> mempty
                in callTool baseUrl (reqId req) name args
        _ -> pure $ Response (reqId req) (RpcError (-32602) "Invalid params")

callTool :: String -> Maybe Value -> Text -> Object -> IO Response
callTool baseUrl rid name args = case name of
    "hoogle_search" -> toolSearch baseUrl rid args
    "hoogle_info"   -> toolInfo baseUrl rid args
    _               -> pure $ Response rid (RpcError (-32602) ("Unknown tool: " <> name))

toolSearch :: String -> Maybe Value -> Object -> IO Response
toolSearch baseUrl rid args =
    case KM.lookup "query" args of
        Just (String query) -> do
            let count = case KM.lookup "count" args of
                    Just (Number n) -> min 100 (max 1 (round n))
                    _               -> 10
            results <- searchHoogle baseUrl query count
            pure $ Response rid $ Result $ object
                ["content" .= [textContent (formatResults results)]]
        _ -> pure $ Response rid (RpcError (-32602) "Missing required 'query' parameter")

toolInfo :: String -> Maybe Value -> Object -> IO Response
toolInfo baseUrl rid args =
    case KM.lookup "query" args of
        Just (String query) -> do
            results <- searchHoogle baseUrl query 1
            let txt = case results of
                    []  -> "No results found."
                    r:_ -> formatInfo r
            pure $ Response rid $ Result $ object
                ["content" .= [textContent txt]]
        _ -> pure $ Response rid (RpcError (-32602) "Missing required 'query' parameter")

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

textContent :: Text -> Value
textContent txt = object ["type" .= ("text" :: Text), "text" .= txt]

formatResults :: [HoogleResult] -> Text
formatResults [] = "No results found."
formatResults results = T.intercalate "\n" $ zipWith formatOne [1 :: Int ..] results
  where
    formatOne n r = T.intercalate "\n" $ filter (not . T.null)
        [ T.pack (show n) <> ". " <> resultItem r <> packageLabel r
        , "   " <> resultUrl r
        , docPreview r
        ]

    packageLabel r = case resultPackage r of
        Just pkg -> "  [" <> refName pkg <> "]"
        Nothing  -> ""

    docPreview r
        | T.null (resultDocs r) = ""
        | T.length (resultDocs r) > 150 =
            "   " <> T.take 150 (resultDocs r) <> "..."
        | otherwise = "   " <> resultDocs r

formatInfo :: HoogleResult -> Text
formatInfo r = T.intercalate "\n" $ filter (not . T.null)
    [ resultItem r
    , ""
    , maybe "" (\p -> "Package: " <> refName p) (resultPackage r)
    , maybe "" (\m -> "Module: " <> refName m) (resultModule r)
    , "URL: " <> resultUrl r
    , ""
    , resultDocs r
    ]
