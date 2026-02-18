{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Type-driven rendering for the notebook: Renderable typeclass and
-- dynamic dispatch for GHCi-evaluated results.
module QEC.Notebook.Renderable
  ( Renderable(..)
  , RenderOutput(..)
  , renderDynamic
  , renderToString
  ) where

import Data.Aeson (Value, object, toJSON, (.=), encode)
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (groupBy, nub)
import Data.Proxy (Proxy(..))
import Data.Text (Text, pack)
import Data.Typeable (Typeable, typeOf)
import Data.Dynamic (Dynamic, fromDynamic)

import QEC.Code.CSS (CSSCode(..), CSSCodeError(..), cssDistance, cssHX, cssHZ, cssNumLogical, cssNumQubits)
import QEC.Export ()
import QEC.GF2 (GF2(..), bvWeight)
import QEC.GF2.Gauss (rank)
import QEC.GF2.Matrix (BinMatrix, bmGetEntry, bmNumCols, bmNumRows, bmGetRow)
import QEC.Noise (PauliChannel(..))
import QEC.Noise.CatQubit (CatQubitParams(..), catQubitChannel, cqAlphaSq, cqGamma, cqKappa1, cqKappa2, cqTCycle)
import QEC.Notebook (SweepResult(..))
import QEC.Resource (ResourceEstimate(..))
import QEC.Simulation (SimResult(..), logicalErrorRate, simLogicalErrors, simTotalTrials)

------------------------------------------------------------------------
-- Renderable typeclass
------------------------------------------------------------------------

-- | A type that the notebook knows how to render.
class (Typeable a, Show a) => Renderable a where
  renderKey :: proxy a -> Text
  renderJSON :: a -> Value

-- | The output produced by the rendering dispatch layer.
data RenderOutput = RenderOutput
  { roRenderAs  :: Maybe Text
  , roData      :: Maybe Value
  , roShowText  :: String
  , roType      :: String
  }

-- | Serialize a Renderable value as a JSON string containing both
-- the render key and the structured data. Called from the GHCi session
-- via @putStrLn (renderToString ___qecIt)@.
renderToString :: Renderable a => a -> String
renderToString x = BLC.unpack $ encode $ object
  [ Key.fromString "render_as" .= renderKey [x]
  , Key.fromString "data"      .= renderJSON x
  ]

------------------------------------------------------------------------
-- Helpers used by multiple instances
------------------------------------------------------------------------

uniqueRowWeights :: BinMatrix -> [Int]
uniqueRowWeights m = nub [ bvWeight (bmGetRow m i) | i <- [0 .. bmNumRows m - 1] ]

codeLabel :: CSSCode -> String
codeLabel code
  | cssNumQubits code <= 50 =
      "[" ++ show (cssNumQubits code) ++ ", " ++ show (cssNumLogical code) ++ ", " ++ show (cssDistance code) ++ "]"
  | otherwise =
      "[" ++ show (cssNumQubits code) ++ ", " ++ show (cssNumLogical code) ++ "]"

tannerGraphJSON :: BinMatrix -> Value
tannerGraphJSON m = object
  [ Key.fromString "num_checks" .= bmNumRows m
  , Key.fromString "num_qubits" .= bmNumCols m
  , Key.fromString "edges"      .= [ object [ Key.fromString "check" .= r, Key.fromString "qubit" .= c ]
                     | r <- [0 .. bmNumRows m - 1]
                     , c <- [0 .. bmNumCols m - 1]
                     , bmGetEntry m r c == GF2 True
                     ]
  ]

describeError :: CSSCodeError -> Value
describeError (DimensionMismatch nx nz) = object
  [ Key.fromString "kind"    .= pack "dimension_mismatch"
  , Key.fromString "message" .= (pack ("H_X has " ++ show nx ++ " columns but H_Z has " ++ show nz ++ " columns"))
  ]
describeError OrthogonalityViolation = object
  [ Key.fromString "kind"    .= pack "orthogonality_violation"
  , Key.fromString "message" .= pack "H_X · H_Z^T ≠ 0 — these matrices do not define a valid CSS code"
  ]

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

instance Renderable CSSCode where
  renderKey _ = pack "css_code"
  renderJSON code = object
    [ Key.fromString "n"           .= cssNumQubits code
    , Key.fromString "k"           .= cssNumLogical code
    , Key.fromString "d"           .= dist
    , Key.fromString "hx_rows"     .= bmNumRows (cssHX code)
    , Key.fromString "hz_rows"     .= bmNumRows (cssHZ code)
    , Key.fromString "hz_weights"  .= uniqueRowWeights (cssHZ code)
    , Key.fromString "hx_weights"  .= uniqueRowWeights (cssHX code)
    , Key.fromString "label"       .= codeLabel code
    , Key.fromString "rate"        .= (fromIntegral (cssNumLogical code) / fromIntegral (cssNumQubits code) :: Double)
    , Key.fromString "tanner"      .= tanner
    ]
    where
      n = cssNumQubits code
      -- cssDistance is exponential; only compute for small codes
      dist = if n <= 50 then Just (cssDistance code) else (Nothing :: Maybe Int)
      -- Tanner graph SVG is only useful for small codes
      tanner = if n <= 200 then Just (tannerGraphJSON (cssHZ code)) else (Nothing :: Maybe Value)

instance Renderable (Either CSSCodeError CSSCode) where
  renderKey _ = pack "code_construction"
  renderJSON (Right code) = object
    [ Key.fromString "status" .= pack "valid"
    , Key.fromString "code"   .= renderJSON code
    ]
  renderJSON (Left err) = object
    [ Key.fromString "status" .= pack "invalid"
    , Key.fromString "error"  .= describeError err
    ]

instance Renderable CatQubitParams where
  renderKey _ = pack "cat_qubit_params"
  renderJSON params = object
    [ Key.fromString "alpha_sq"        .= cqAlphaSq params
    , Key.fromString "kappa1"           .= cqKappa1 params
    , Key.fromString "kappa2"           .= cqKappa2 params
    , Key.fromString "t_cycle_ns"       .= (cqTCycle params * 1e9)
    , Key.fromString "gamma"            .= cqGamma params
    , Key.fromString "kappa_ratio"      .= (cqKappa1 params / cqKappa2 params)
    , Key.fromString "p_x"              .= px
    , Key.fromString "p_z"              .= pz
    , Key.fromString "p_y"              .= py
    , Key.fromString "bias"             .= (pz / max 1e-30 px)
    , Key.fromString "bias_log10"       .= logBase 10 (pz / max 1e-30 px)
    ]
    where
      PauliChannel px py pz = catQubitChannel params

instance Renderable PauliChannel where
  renderKey _ = pack "pauli_channel"
  renderJSON (PauliChannel px py pz) = object
    [ Key.fromString "p_x"        .= px
    , Key.fromString "p_y"        .= py
    , Key.fromString "p_z"        .= pz
    , Key.fromString "p_total"    .= (px + py + pz)
    , Key.fromString "bias"       .= (pz / max 1e-30 px)
    , Key.fromString "bias_log10" .= logBase 10 (pz / max 1e-30 px)
    ]

instance Renderable SimResult where
  renderKey _ = pack "sim_result"
  renderJSON result = object
    [ Key.fromString "total_trials"   .= simTotalTrials result
    , Key.fromString "logical_errors" .= simLogicalErrors result
    , Key.fromString "logical_rate"   .= rate
    , Key.fromString "std_error"      .= stdErr
    , Key.fromString "ci_lower"       .= max 0 (rate - 1.96 * stdErr)
    , Key.fromString "ci_upper"       .= min 1 (rate + 1.96 * stdErr)
    ]
    where
      rate   = logicalErrorRate result
      n      = fromIntegral (simTotalTrials result)
      stdErr = sqrt (rate * (1 - rate) / n)

instance Renderable [SweepResult] where
  renderKey _ = pack "threshold_plot"
  renderJSON results = object
    [ Key.fromString "series"  .= groupBySeries results
    , Key.fromString "x_label" .= pack "Physical phase-flip rate p_Z"
    , Key.fromString "y_label" .= pack "Logical error rate p_L"
    , Key.fromString "x_scale" .= pack "linear"
    , Key.fromString "y_scale" .= pack "log"
    ]

groupBySeries :: [SweepResult] -> Value
groupBySeries results = toJSON
  [ object
    [ Key.fromString "label"  .= label
    , Key.fromString "points" .= [ object
        [ Key.fromString "x"       .= swPhysicalPZ r
        , Key.fromString "y"       .= swLogicalErr r
        , Key.fromString "y_lower" .= max 1e-15 (swLogicalErr r - 1.96 * swStdError r)
        , Key.fromString "y_upper" .= (swLogicalErr r + 1.96 * swStdError r)
        , Key.fromString "trials"  .= swNumTrials r
        , Key.fromString "errors"  .= swNumErrors r
        ]
      | r <- pts
      ]
    ]
  | (label, pts) <- groupByLabel results
  ]
  where
    groupByLabel rs = map (\g -> (swCodeLabel (head g), g)) (groupBy (\a b -> swCodeLabel a == swCodeLabel b) rs)

instance Renderable ResourceEstimate where
  renderKey _ = pack "resource_estimate"
  renderJSON = toJSON

instance Renderable [ResourceEstimate] where
  renderKey _ = pack "resource_comparison"
  renderJSON estimates = object
    [ Key.fromString "columns" .= ([ pack "code_family", pack "total_qubits", pack "data_qubits"
                    , pack "syndrome_qubits", pack "routing_qubits"
                    , pack "factory_qubits", pack "code_distance"
                    , pack "runtime_seconds", pack "num_factories"
                    , pack "logical_error_per_cycle"
                    ] :: [Text])
    , Key.fromString "rows"    .= map toJSON estimates
    ]

instance Renderable BinMatrix where
  renderKey _ = pack "bin_matrix"
  renderJSON m = object
    [ Key.fromString "rows"    .= bmNumRows m
    , Key.fromString "cols"    .= bmNumCols m
    , Key.fromString "density" .= density
    , Key.fromString "rank"    .= rank m
    , Key.fromString "display" .= displayData
    ]
    where
      total   = bmNumRows m * bmNumCols m
      ones    = sum [ bvWeight (bmGetRow m r) | r <- [0 .. bmNumRows m - 1] ]
      density = (if total == 0 then 0.0 else fromIntegral ones / fromIntegral total) :: Double
      displayData
        | bmNumRows m <= 32 && bmNumCols m <= 64 =
            Just [ [ if bmGetEntry m r c == GF2 True then (1 :: Int) else 0
                   | c <- [0 .. bmNumCols m - 1] ]
                 | r <- [0 .. bmNumRows m - 1] ]
        | otherwise = Nothing

------------------------------------------------------------------------
-- Dynamic dispatch
------------------------------------------------------------------------

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x : _) = Just x
firstJust (Nothing : xs) = firstJust xs

tryRender :: forall a. Renderable a => Dynamic -> Maybe RenderOutput
tryRender d = case fromDynamic @a d of
  Just val -> Just RenderOutput
    { roRenderAs = Just (renderKey (Proxy @a))
    , roData     = Just (renderJSON val)
    , roShowText = show val
    , roType     = show (typeOf val)
    }
  Nothing -> Nothing

-- | Attempt to render a Dynamic value using registered instances.
-- Tries each known type in order; falls back to Show.
renderDynamic :: Dynamic -> String -> String -> RenderOutput
renderDynamic dyn showStr typeStr =
  case tryAll dyn of
    Just ro -> ro
    Nothing -> RenderOutput Nothing Nothing showStr typeStr
  where
    tryAll d = firstJust
      [ tryRender @CSSCode d
      , tryRender @(Either CSSCodeError CSSCode) d
      , tryRender @CatQubitParams d
      , tryRender @PauliChannel d
      , tryRender @SimResult d
      , tryRender @[SweepResult] d
      , tryRender @ResourceEstimate d
      , tryRender @[ResourceEstimate] d
      , tryRender @BinMatrix d
      ]
