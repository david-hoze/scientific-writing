module Test.Unit.Eval (tests) where

import Test.Tasty
import Test.Tasty.HUnit
import Data.Aeson ((.=), encode, eitherDecode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

import Eval (classifyCell, CellKind(..), buildResultMsg, buildDeclOkMsg, buildErrorMsg)
import Protocol (ResultMsg(..), ResultStatus(..), ErrorType(..))
import Render (cleanTypeStr)
import QEC.Notebook.Renderable (RenderOutput(..))

tests :: TestTree
tests = testGroup "Eval"
  [ classifyCellTests
  , messageBuilderTests
  , cleanTypeStrTests
  ]

------------------------------------------------------------------------
-- § 3.1  Cell classification
------------------------------------------------------------------------

classifyCellTests :: TestTree
classifyCellTests = testGroup "classifyCell"
  [ t "ldpcCatCode 0"                     Expression
  , t "42 :: Int"                          Expression
  , t "let x = 5"                          Declaration
  , t "let quickConfig = SimConfig 500 defaultBPConfig 1" Declaration
  , t "import Data.List"                   Declaration
  , t "data Foo = Bar"                     Declaration
  , t "type X = Int"                       Declaration
  , t "newtype Wrapper = W Int"            Declaration
  , t "class MyClass a where"             Declaration
  , t "instance Show Foo where"           Declaration
  , t ":type 42"                           GHCiCommand
  , t ":info Maybe"                        GHCiCommand
  , t ":set -XOverloadedStrings"           GHCiCommand
  , t "  let x = 5"                        Declaration   -- leading whitespace
  , t "  42"                               Expression   -- leading whitespace
  , t "{-# LANGUAGE GADTs #-}"            Declaration
  ]
  where
    t :: String -> CellKind -> TestTree
    t input expected = testCase (show input) $
      classifyCell input @?= expected

------------------------------------------------------------------------
-- § 3.2  Message builders
------------------------------------------------------------------------

messageBuilderTests :: TestTree
messageBuilderTests = testGroup "message builders"
  [ testCase "buildDeclOkMsg fields" $ do
      let msg = buildDeclOkMsg "c-1" 5
      resStatus msg   @?= DeclOk
      resRenderAs msg @?= Nothing
      resShowText msg @?= T.pack ""

  , testCase "buildDeclOkMsg round-trip JSON" $ do
      let msg = buildDeclOkMsg "c-1" 5
          json = encode msg
      case eitherDecode json :: Either String Aeson.Value of
        Left err -> assertFailure ("Failed to decode: " ++ err)
        Right val -> do
          assertJsonField val "status" (Aeson.String "decl_ok")
          assertJsonFieldNull val "render_as"

  , testCase "buildErrorMsg fields" $ do
      let msg = buildErrorMsg "c-2" RuntimeException "boom" 10
      resStatus msg       @?= Error
      resRenderAs msg     @?= Just (T.pack "error")
      resErrorType msg    @?= Just RuntimeException
      resErrorMessage msg @?= Just (T.pack "boom")

  , testCase "buildErrorMsg round-trip JSON" $ do
      let msg = buildErrorMsg "c-2" RuntimeException "boom" 10
          json = encode msg
      case eitherDecode json :: Either String Aeson.Value of
        Left err -> assertFailure ("Failed to decode: " ++ err)
        Right val -> do
          assertJsonField val "status" (Aeson.String "error")
          assertJsonField val "render_as" (Aeson.String "error")
          case getJsonField "data" val of
            Just (Aeson.Object o) -> do
              KM.lookup (Key.fromText "error_type") o @?= Just (Aeson.String "runtime_exception")
              KM.lookup (Key.fromText "message") o @?= Just (Aeson.String "boom")
            _ -> assertFailure "Expected 'data' to be an object"

  , testCase "buildResultMsg with mock RenderOutput" $ do
      let ro = RenderOutput
                 { roRenderAs = Just (T.pack "css_code")
                 , roData     = Just (Aeson.object [Key.fromString "n" .= (3 :: Int)])
                 , roShowText = "CSSCode { ... }"
                 , roType     = "CSSCode"
                 }
          msg = buildResultMsg "c-3" 42 ro
      resStatus msg     @?= Ok
      resHaskellType msg @?= Just (T.pack "CSSCode")
      resRenderAs msg   @?= Just (T.pack "css_code")
      resShowText msg   @?= T.pack "CSSCode { ... }"

  , testCase "buildResultMsg round-trip JSON" $ do
      let ro = RenderOutput
                 { roRenderAs = Just (T.pack "css_code")
                 , roData     = Just (Aeson.object [Key.fromString "n" .= (3 :: Int)])
                 , roShowText = "CSSCode { ... }"
                 , roType     = "CSSCode"
                 }
          msg = buildResultMsg "c-3" 42 ro
          json = encode msg
      case eitherDecode json :: Either String Aeson.Value of
        Left err -> assertFailure ("Failed to decode: " ++ err)
        Right val -> do
          assertJsonField val "status" (Aeson.String "ok")
          assertJsonField val "haskell_type" (Aeson.String "CSSCode")
          assertJsonField val "render_as" (Aeson.String "css_code")
  ]

------------------------------------------------------------------------
-- § 3.4  cleanTypeStr
------------------------------------------------------------------------

cleanTypeStrTests :: TestTree
cleanTypeStrTests = testGroup "cleanTypeStr"
  [ t "___qecIt :: CSSCode"                          "CSSCode"
  , t "___qecIt :: [SweepResult]"                     "[SweepResult]"
  , t "___qecIt :: Either CSSCodeError CSSCode"       "Either CSSCodeError CSSCode"
  , t "CSSCode"                                       "CSSCode"
  , t "  ___qecIt :: Int  "                           "Int"
  ]
  where
    t :: String -> String -> TestTree
    t input expected = testCase (show input) $
      cleanTypeStr input @?= expected

------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------

getJsonField :: Text -> Aeson.Value -> Maybe Aeson.Value
getJsonField k (Aeson.Object o) = KM.lookup (Key.fromText k) o
getJsonField _ _                = Nothing

assertJsonField :: Aeson.Value -> Text -> Aeson.Value -> Assertion
assertJsonField obj field expected =
  case getJsonField field obj of
    Nothing  -> assertFailure ("Missing field: " ++ T.unpack field)
    Just val -> val @?= expected

assertJsonFieldNull :: Aeson.Value -> Text -> Assertion
assertJsonFieldNull obj field =
  case getJsonField field obj of
    Nothing           -> return ()  -- absent is as good as null
    Just Aeson.Null   -> return ()
    Just val          -> assertFailure
      ("Expected null for '" ++ T.unpack field ++ "', got: " ++ show val)
