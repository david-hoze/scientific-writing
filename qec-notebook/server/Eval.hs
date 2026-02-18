-- | Cell classification and evaluation pipeline.
module Eval
  ( classifyCell
  , CellKind(..)
  , buildResultMsg
  , buildDeclOkMsg
  , buildErrorMsg
  ) where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Text (Text, pack)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=))
import qualified Data.Aeson.Key as Key

import Protocol (ResultMsg(..), ResultStatus(..), ErrorType(..))
import QEC.Notebook.Renderable (RenderOutput(..))

data CellKind = Declaration | Expression | GHCiCommand
  deriving (Show, Eq)

classifyCell :: String -> CellKind
classifyCell src
  | any (`isPrefixOf` stripped) declPrefixes = Declaration
  | ":" `isPrefixOf` stripped                = GHCiCommand
  | otherwise                                = Expression
  where
    stripped = dropWhile isSpace src
    declPrefixes =
      [ "let ", "import ", "data ", "type ", "newtype "
      , "class ", "instance ", "module ", "{-#"
      ]

-- | Build a result message from RenderOutput (expression evaluated successfully).
buildResultMsg :: Text -> Int -> RenderOutput -> ResultMsg
buildResultMsg cellId elapsedMs ro =
  ResultMsg
    { resCellId        = cellId
    , resStatus        = Ok
    , resHaskellType   = Just (pack (roType ro))
    , resRenderAs      = roRenderAs ro
    , resData          = roData ro
    , resShowText      = pack (roShowText ro)
    , resElapsedMs     = elapsedMs
    , resErrorType     = Nothing
    , resErrorMessage  = Nothing
    }

-- | Declaration or GHCi command completed (no result to render).
buildDeclOkMsg :: Text -> Int -> ResultMsg
buildDeclOkMsg cellId elapsedMs =
  ResultMsg
    { resCellId        = cellId
    , resStatus        = DeclOk
    , resHaskellType   = Nothing
    , resRenderAs      = Nothing
    , resData          = Nothing
    , resShowText      = pack ""
    , resElapsedMs     = elapsedMs
    , resErrorType     = Nothing
    , resErrorMessage  = Nothing
    }

-- | Evaluation failed.
buildErrorMsg :: Text -> ErrorType -> String -> Int -> ResultMsg
buildErrorMsg cellId errType message elapsedMs =
  ResultMsg
    { resCellId        = cellId
    , resStatus        = Error
    , resHaskellType   = Nothing
    , resRenderAs      = Just (pack "error")
    , resData          = Just (Aeson.object
        [ Key.fromString "error_type"  .= errorTypeText errType
        , Key.fromString "message"     .= pack message
        , Key.fromString "suggestions" .= ([] :: [Text])
        ])
    , resShowText      = pack ""
    , resElapsedMs     = elapsedMs
    , resErrorType     = Just errType
    , resErrorMessage  = Just (pack message)
    }
  where
    errorTypeText :: ErrorType -> Text
    errorTypeText TypeError        = pack "type_error"
    errorTypeText ParseError       = pack "parse_error"
    errorTypeText NotInScope       = pack "not_in_scope"
    errorTypeText RuntimeException = pack "runtime_exception"
    errorTypeText Timeout          = pack "timeout"
    errorTypeText Cancelled        = pack "cancelled"
