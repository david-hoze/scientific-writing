-- | JSON and CSV export for resource estimates.
module QEC.Export
  ( exportJSON
  , exportCSV
  ) where

import Data.Aeson (ToJSON(..), object, (.=), encode)
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC

import QEC.Resource

------------------------------------------------------------------------
-- JSON export
------------------------------------------------------------------------

instance ToJSON ResourceEstimate where
  toJSON re = object
    [ Key.fromString "data_qubits"            .= reDataQubits re
    , Key.fromString "syndrome_qubits"        .= reSyndromeQubits re
    , Key.fromString "routing_qubits"         .= reRoutingQubits re
    , Key.fromString "factory_qubits"         .= reFactoryQubits re
    , Key.fromString "total_qubits"           .= reTotalQubits re
    , Key.fromString "runtime_seconds"        .= reRuntimeSeconds re
    , Key.fromString "code_distance"          .= reCodeDistance re
    , Key.fromString "logical_error_per_cycle" .= reLogicalErrorPerCycle re
    , Key.fromString "num_factories"          .= reNumFactories re
    , Key.fromString "code_family"            .= reCodeFamily re
    ]

-- | Export a list of resource estimates as a JSON array.
exportJSON :: [ResourceEstimate] -> BL.ByteString
exportJSON = encode

------------------------------------------------------------------------
-- CSV export
------------------------------------------------------------------------

-- | Export resource estimates as CSV with headers.
exportCSV :: [ResourceEstimate] -> BL.ByteString
exportCSV results = BLC.unlines (BLC.pack header : map formatRow results)
  where
    header = "data_qubits,syndrome_qubits,routing_qubits,factory_qubits,"
          ++ "total_qubits,runtime_seconds,code_distance,"
          ++ "logical_error_per_cycle,num_factories,code_family"

    formatRow re = BLC.pack $ intercalate' ","
      [ show (reDataQubits re)
      , show (reSyndromeQubits re)
      , show (reRoutingQubits re)
      , show (reFactoryQubits re)
      , show (reTotalQubits re)
      , show (reRuntimeSeconds re)
      , show (reCodeDistance re)
      , show (reLogicalErrorPerCycle re)
      , show (reNumFactories re)
      , show (reCodeFamily re)
      ]

    intercalate' _ [] = ""
    intercalate' _ [x] = x
    intercalate' sep (x:xs) = x ++ sep ++ intercalate' sep xs
