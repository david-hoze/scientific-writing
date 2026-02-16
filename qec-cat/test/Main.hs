module Main (main) where

import Test.Tasty
import qualified QEC.GF2.Test
import qualified QEC.GF2.Matrix.Test
import qualified QEC.GF2.Gauss.Test
import qualified QEC.Symplectic.Test
import qualified QEC.Code.CSS.Test
import qualified QEC.Code.Surface.Test
import qualified QEC.Code.LDPCCat.Test
import qualified QEC.Noise.Test
import qualified QEC.Decoder.BP.Test
import qualified QEC.Decoder.OSD.Test
import qualified QEC.Stabilizer.Tableau.Test
import qualified QEC.Simulation.Test
import qualified QEC.Resource.Test
import qualified QEC.Export.Test

main :: IO ()
main = defaultMain $ testGroup "qec-cat"
  [ QEC.GF2.Test.tests
  , QEC.GF2.Matrix.Test.tests
  , QEC.GF2.Gauss.Test.tests
  , QEC.Symplectic.Test.tests
  , QEC.Code.CSS.Test.tests
  , QEC.Code.Surface.Test.tests
  , QEC.Code.LDPCCat.Test.tests
  , QEC.Noise.Test.tests
  , QEC.Decoder.BP.Test.tests
  , QEC.Decoder.OSD.Test.tests
  , QEC.Stabilizer.Tableau.Test.tests
  , QEC.Simulation.Test.tests
  , QEC.Resource.Test.tests
  , QEC.Export.Test.tests
  ]
