-- | Server-side rendering dispatch.
--
-- After binding an expression to @___qecIt@ via let, we ask GHCi to call
-- @renderToString ___qecIt@. If the type has a Renderable instance, this
-- produces JSON with render_as and data fields. Otherwise we fall back
-- to @show@.
module Render
  ( renderViaGHCi
  , cleanTypeStr
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BLC

import Session (Session, evalInSession)
import QEC.Notebook.Renderable (RenderOutput(..))

-- | Attempt structured rendering via GHCi, falling back to show.
renderViaGHCi :: Session -> String -> IO RenderOutput
renderViaGHCi session rawTypeStr = do
  let typeStr = cleanTypeStr rawTypeStr
  -- Try renderToString first (fast path for Renderable types)
  jsonOut <- evalInSession session "putStrLn (renderToString ___qecIt)"
  let trimmed = strip jsonOut
  case Aeson.decode (BLC.pack trimmed) >>= extractFields of
    Just (renderAs, dat) ->
      return (RenderOutput (Just renderAs) (Just dat) "" typeStr)
    Nothing -> do
      -- No Renderable instance; fall back to truncated show
      showOut <- evalInSession session "putStrLn (take 2000 (show ___qecIt))"
      return (RenderOutput Nothing Nothing (strip showOut) typeStr)

-- | Extract render_as and data fields from the parsed JSON object.
extractFields :: Aeson.Value -> Maybe (Text, Aeson.Value)
extractFields (Aeson.Object o) = do
  Aeson.String renderAs <- KM.lookup (Key.fromString "render_as") o
  dat                   <- KM.lookup (Key.fromString "data") o
  return (renderAs, dat)
extractFields _ = Nothing

-- | Strip the variable name prefix from GHCi's ":type" output.
-- GHCi returns "___qecIt :: CSSCode" — we want just "CSSCode".
cleanTypeStr :: String -> String
cleanTypeStr s =
  let trimmed = strip s
  in case Prelude.break (== ':') trimmed of
    (_, ':':':':rest) -> strip rest
    _ -> trimmed

strip :: String -> String
strip = f . f
  where f = reverse . dropWhile isSpace
