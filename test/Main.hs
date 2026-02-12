module Main (main) where

import Data.Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (isJust, isNothing)
import Data.Text qualified as T
import System.Exit (exitFailure, exitSuccess)

import HoogleMCP.Hoogle
import HoogleMCP.Server

main :: IO ()
main = do
    results <- sequence
        [ group "Request parsing" requestParsingTests
        , group "Response encoding" responseEncodingTests
        , group "HoogleResult parsing" hoogleResultParsingTests
        , group "formatResults" formatResultsTests
        , group "formatInfo" formatInfoTests
        , group "initializeResult" initializeResultTests
        , group "toolsListResult" toolsListResultTests
        ]
    if and results
        then putStrLn "\nAll tests passed." >> exitSuccess
        else putStrLn "\nSome tests failed." >> exitFailure

group :: String -> [IO Bool] -> IO Bool
group name tests = do
    putStrLn $ "\n== " <> name <> " =="
    and <$> sequence tests

assert :: String -> Bool -> IO Bool
assert label True  = putStrLn ("  PASS: " <> label) >> pure True
assert label False = putStrLn ("  FAIL: " <> label) >> pure False

--------------------------------------------------------------------------------
-- Request parsing
--------------------------------------------------------------------------------

requestParsingTests :: [IO Bool]
requestParsingTests =
    [ testParseValidRequest
    , testParseRequestWithoutId
    , testParseRequestWithoutParams
    , testParseInvalidRequest
    ]

testParseValidRequest :: IO Bool
testParseValidRequest =
    let json = "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"initialize\",\"params\":{}}"
    in case eitherDecode json :: Either String Request of
        Right req -> assert "valid request parses" $
            reqMethod req == "initialize" && isJust (reqId req) && isJust (reqParams req)
        Left _ -> assert "valid request parses" False

testParseRequestWithoutId :: IO Bool
testParseRequestWithoutId =
    let json = "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}"
    in case eitherDecode json :: Either String Request of
        Right req -> assert "request without id (notification)" $
            reqMethod req == "notifications/initialized" && isNothing (reqId req)
        Left _ -> assert "request without id (notification)" False

testParseRequestWithoutParams :: IO Bool
testParseRequestWithoutParams =
    let json = "{\"jsonrpc\":\"2.0\",\"id\":2,\"method\":\"ping\"}"
    in case eitherDecode json :: Either String Request of
        Right req -> assert "request without params" $
            reqMethod req == "ping" && isNothing (reqParams req)
        Left _ -> assert "request without params" False

testParseInvalidRequest :: IO Bool
testParseInvalidRequest =
    let json = "{\"jsonrpc\":\"2.0\",\"id\":1}"
    in case eitherDecode json :: Either String Request of
        Left _  -> assert "invalid request (missing method) fails" True
        Right _ -> assert "invalid request (missing method) fails" False

--------------------------------------------------------------------------------
-- Response encoding
--------------------------------------------------------------------------------

responseEncodingTests :: [IO Bool]
responseEncodingTests =
    [ testEncodeSuccessResponse
    , testEncodeErrorResponse
    ]

testEncodeSuccessResponse :: IO Bool
testEncodeSuccessResponse =
    let resp = Response (Just (Number 1)) (Result (object ["ok" .= True]))
        json = encode resp
    in case decode json :: Maybe Value of
        Just (Object obj) -> assert "success response has result field" $
            isJust (KM.lookup "result" obj)
                && KM.lookup "jsonrpc" obj == Just (String "2.0")
        _ -> assert "success response has result field" False

testEncodeErrorResponse :: IO Bool
testEncodeErrorResponse =
    let resp = Response (Just (Number 1)) (RpcError (-32601) "Unknown method")
        json = encode resp
    in case decode json :: Maybe Value of
        Just (Object obj) -> assert "error response has error field" $
            isJust (KM.lookup "error" obj)
        _ -> assert "error response has error field" False

--------------------------------------------------------------------------------
-- HoogleResult parsing
--------------------------------------------------------------------------------

hoogleResultParsingTests :: [IO Bool]
hoogleResultParsingTests =
    [ testParseFullResult
    , testParseMinimalResult
    , testParseResultList
    ]

sampleResultJson :: LBS.ByteString
sampleResultJson =
    "{\"url\":\"https://hackage.haskell.org/package/base/docs/Prelude.html#v:map\",\
    \\"module\":{\"url\":\"https://hackage.haskell.org/package/base/docs/Prelude.html\",\"name\":\"Prelude\"},\
    \\"package\":{\"url\":\"https://hackage.haskell.org/package/base\",\"name\":\"base\"},\
    \\"item\":\"map :: (a -> b) -> [a] -> [b]\",\
    \\"docs\":\"Apply a function to each element of a list.\"}"

testParseFullResult :: IO Bool
testParseFullResult =
    case eitherDecode sampleResultJson :: Either String HoogleResult of
        Right r -> assert "full result parses correctly" $
            resultItem r == "map :: (a -> b) -> [a] -> [b]"
                && isJust (resultModule r)
                && isJust (resultPackage r)
                && resultDocs r == "Apply a function to each element of a list."
        Left err -> assert ("full result parses correctly: " <> err) False

testParseMinimalResult :: IO Bool
testParseMinimalResult =
    let json = "{\"url\":\"https://example.com\",\"item\":\"foo :: Int\"}"
    in case eitherDecode json :: Either String HoogleResult of
        Right r -> assert "minimal result (no module/package/docs)" $
            resultItem r == "foo :: Int"
                && isNothing (resultModule r)
                && isNothing (resultPackage r)
                && resultDocs r == ""
        Left err -> assert ("minimal result: " <> err) False

testParseResultList :: IO Bool
testParseResultList =
    let json = "[" <> sampleResultJson <> "," <> sampleResultJson <> "]"
    in case eitherDecode json :: Either String [HoogleResult] of
        Right rs -> assert "list of results parses" $ length rs == 2
        Left err -> assert ("list of results: " <> err) False

--------------------------------------------------------------------------------
-- formatResults
--------------------------------------------------------------------------------

formatResultsTests :: [IO Bool]
formatResultsTests =
    [ testFormatResultsEmpty
    , testFormatResultsNumbered
    , testFormatResultsTruncatesDocs
    ]

sampleResult :: HoogleResult
sampleResult = case eitherDecode sampleResultJson of
    Right r  -> r
    Left err -> error err

testFormatResultsEmpty :: IO Bool
testFormatResultsEmpty =
    assert "empty results → 'No results found.'" $
        formatResults [] == "No results found."

testFormatResultsNumbered :: IO Bool
testFormatResultsNumbered =
    let txt = formatResults [sampleResult, sampleResult]
    in assert "results are numbered starting at 1" $
        T.isInfixOf "1. " txt && T.isInfixOf "2. " txt

testFormatResultsTruncatesDocs :: IO Bool
testFormatResultsTruncatesDocs =
    let longDoc = T.replicate 200 "x"
        r = sampleResult { resultDocs = longDoc }
        txt = formatResults [r]
    in assert "long docs are truncated with '...'" $
        T.isInfixOf "..." txt

--------------------------------------------------------------------------------
-- formatInfo
--------------------------------------------------------------------------------

formatInfoTests :: [IO Bool]
formatInfoTests =
    [ testFormatInfoContainsItem
    , testFormatInfoContainsPackage
    , testFormatInfoContainsModule
    , testFormatInfoContainsUrl
    ]

testFormatInfoContainsItem :: IO Bool
testFormatInfoContainsItem =
    assert "formatInfo includes item signature" $
        T.isInfixOf "map :: (a -> b) -> [a] -> [b]" (formatInfo sampleResult)

testFormatInfoContainsPackage :: IO Bool
testFormatInfoContainsPackage =
    assert "formatInfo includes package name" $
        T.isInfixOf "Package: base" (formatInfo sampleResult)

testFormatInfoContainsModule :: IO Bool
testFormatInfoContainsModule =
    assert "formatInfo includes module name" $
        T.isInfixOf "Module: Prelude" (formatInfo sampleResult)

testFormatInfoContainsUrl :: IO Bool
testFormatInfoContainsUrl =
    assert "formatInfo includes URL" $
        T.isInfixOf "URL: https://hackage.haskell.org" (formatInfo sampleResult)

--------------------------------------------------------------------------------
-- initializeResult
--------------------------------------------------------------------------------

initializeResultTests :: [IO Bool]
initializeResultTests =
    [ testInitializeHasProtocolVersion
    , testInitializeHasServerInfo
    ]

testInitializeHasProtocolVersion :: IO Bool
testInitializeHasProtocolVersion =
    case initializeResult of
        Object obj -> assert "initializeResult has protocolVersion" $
            isJust (KM.lookup "protocolVersion" obj)
        _ -> assert "initializeResult is an object" False

testInitializeHasServerInfo :: IO Bool
testInitializeHasServerInfo =
    case initializeResult of
        Object obj -> assert "initializeResult has serverInfo" $
            isJust (KM.lookup "serverInfo" obj)
        _ -> assert "initializeResult is an object" False

--------------------------------------------------------------------------------
-- toolsListResult
--------------------------------------------------------------------------------

toolsListResultTests :: [IO Bool]
toolsListResultTests =
    [ testToolsListHasTools
    , testToolsListHasTwoTools
    ]

testToolsListHasTools :: IO Bool
testToolsListHasTools =
    case toolsListResult of
        Object obj -> assert "toolsListResult has 'tools' key" $
            isJust (KM.lookup "tools" obj)
        _ -> assert "toolsListResult is an object" False

testToolsListHasTwoTools :: IO Bool
testToolsListHasTwoTools =
    case toolsListResult of
        Object obj
            | Just (Array ts) <- KM.lookup "tools" obj ->
                assert "toolsListResult has 2 tools" $ length ts == 2
        _ -> assert "toolsListResult has 2 tools" False
