-- | JSON message types for the WebSocket protocol.
module Protocol
  ( ClientMsg(..)
  , ServerMsg(..)
  , EvalReq(..)
  , ResultStatus(..)
  , ResultMsg(..)
  , ProgressMsg(..)
  , ReadyMsg(..)
  , ErrorType(..)
  ) where

import Data.Aeson (FromJSON, ToJSON(..), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key

-- | Client → Server message variants.
data ClientMsg
  = CM_eval !EvalReq
  | CM_cancel !Text
  | CM_reset
  | CM_save !FilePath !NotebookData
  | CM_load !FilePath
  deriving (Show)

data EvalReq = EvalReq
  { evalCellId :: !Text
  , evalSource  :: !Text
  } deriving (Show)

data NotebookData = NotebookData
  { nbCells :: [CellData]
  } deriving (Show)

data CellData = CellData
  { cellId     :: !Text
  , cellSource :: !Text
  } deriving (Show)

-- | Server → Client message variants.
data ServerMsg
  = SM_ready !ReadyMsg
  | SM_result !ResultMsg
  | SM_saved !FilePath
  | SM_notebook !NotebookData
  deriving (Show)

data ResultStatus = Ok | DeclOk | Error
  deriving (Show, Eq)

data ResultMsg = ResultMsg
  { resCellId        :: !Text
  , resStatus        :: !ResultStatus
  , resHaskellType   :: !(Maybe Text)
  , resRenderAs      :: !(Maybe Text)
  , resData          :: !(Maybe Aeson.Value)
  , resShowText      :: !Text
  , resElapsedMs     :: !Int
  , resErrorType     :: !(Maybe ErrorType)
  , resErrorMessage  :: !(Maybe Text)
  } deriving (Show)

data ProgressMsg = ProgressMsg
  { progCellId     :: !Text
  , progRenderAs   :: !Text
  , progData       :: !Aeson.Value
  , progElapsedMs  :: !Int
  } deriving (Show)

data ReadyMsg = ReadyMsg
  { readyVersion :: !Text
  , readyPrelude :: ![Text]
  } deriving (Show)

data ErrorType
  = TypeError | ParseError | NotInScope
  | RuntimeException | Timeout | Cancelled
  deriving (Show, Eq)

-- | JSON decoding for client messages (type tag + payload).
instance FromJSON ClientMsg where
  parseJSON = withObject "ClientMsg" $ \o -> do
    ty <- o .: "type"
    case (ty :: Text) of
      "eval"   -> CM_eval <$> (EvalReq <$> o .: "cell_id" <*> o .: "source")
      "cancel" -> CM_cancel <$> o .: "cell_id"
      "reset"  -> pure CM_reset
      "save"   -> CM_save <$> o .: "path" <*> (o .: "notebook" >>= parseNotebookData)
      "load"   -> CM_load <$> o .: "path"
      _        -> fail ("unknown client message type: " ++ show ty)

parseNotebookData :: Aeson.Value -> Parser NotebookData
parseNotebookData = withObject "NotebookData" $ \o ->
  NotebookData <$> o .: "cells"

instance FromJSON CellData where
  parseJSON = withObject "CellData" $ \o ->
    CellData <$> o .: "cell_id" <*> o .: "source"

instance FromJSON EvalReq where
  parseJSON = withObject "EvalReq" $ \o ->
    EvalReq <$> o .: "cell_id" <*> o .: "source"

instance ToJSON ResultStatus where
  toJSON Ok      = Aeson.String "ok"
  toJSON DeclOk  = Aeson.String "decl_ok"
  toJSON Error   = Aeson.String "error"

instance ToJSON ResultMsg where
  toJSON r = object
    [ "type"          .= ("result" :: Text)
    , "cell_id"       .= resCellId r
    , "status"        .= resStatus r
    , "haskell_type"  .= resHaskellType r
    , "render_as"     .= resRenderAs r
    , "data"          .= resData r
    , "show_text"     .= resShowText r
    , "elapsed_ms"    .= resElapsedMs r
    ]

instance ToJSON ProgressMsg where
  toJSON p = object
    [ "type"        .= ("progress" :: Text)
    , "cell_id"     .= progCellId p
    , "render_as"   .= progRenderAs p
    , "data"        .= progData p
    , "elapsed_ms"  .= progElapsedMs p
    ]

instance ToJSON ReadyMsg where
  toJSON rd = object
    [ "type"    .= ("ready" :: Text)
    , "version" .= readyVersion rd
    , "prelude"  .= readyPrelude rd
    ]

instance ToJSON ErrorType where
  toJSON TypeError        = Aeson.String "type_error"
  toJSON ParseError       = Aeson.String "parse_error"
  toJSON NotInScope       = Aeson.String "not_in_scope"
  toJSON RuntimeException = Aeson.String "runtime_exception"
  toJSON Timeout          = Aeson.String "timeout"
  toJSON Cancelled        = Aeson.String "cancelled"

instance ToJSON ServerMsg where
  toJSON (SM_ready r)     = toJSON r
  toJSON (SM_result r)    = toJSON r
  toJSON (SM_saved path)  = object [ Key.fromString "type" .= ("saved" :: Text), Key.fromString "path" .= path ]
  toJSON (SM_notebook nb) = object [ Key.fromString "type" .= ("notebook" :: Text), Key.fromString "notebook" .= nb ]

instance ToJSON NotebookData where
  toJSON nd = object [ Key.fromString "cells" .= nbCells nd ]

instance ToJSON CellData where
  toJSON c = object [ Key.fromString "cell_id" .= cellId c, Key.fromString "source" .= cellSource c ]
