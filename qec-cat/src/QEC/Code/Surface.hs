-- | Rotated surface code.
--
-- The rotated surface code @[[d^2, 1, d]]@ uses a d x d grid of data qubits
-- with X and Z stabilizers on alternating plaquettes.
module QEC.Code.Surface
  ( surfaceCode
  ) where

import QEC.GF2
import QEC.GF2.Matrix
import QEC.Code.CSS

-- | Construct a rotated surface code of distance @d@ (d >= 2).
-- Parameters: @[[d^2, 1, d]]@.
--
-- Uses the standard construction where data qubits are on vertices of a
-- d x d grid. Stabilizers are "face" operators on the dual lattice.
-- We use the formulation from Horsman et al. / Tomita-Svore where:
--
-- - Qubits at (r, c) for 0 <= r < d, 0 <= c < d (row-major indexing)
-- - X stabilizers on "white" plaquettes
-- - Z stabilizers on "black" plaquettes
-- - Boundary conditions chosen so k = 1
surfaceCode :: Int -> CSSCode
surfaceCode d
  | d < 2     = error "surfaceCode: d must be >= 2"
  | otherwise = case mkCSSCode hx hz of
      Left err   -> error $ "surfaceCode: unexpected error: " ++ show err
      Right code -> code
  where
    n = d * d

    -- Make a stabilizer row from a list of (r,c) positions
    makeStab positions = bvFromList
      [ GF2 ((r, c) `elem` positions)
      | r <- [0 .. d - 1], c <- [0 .. d - 1]
      ]

    -- The standard rotated surface code places stabilizers on two
    -- types of plaquettes. We follow the convention where:
    --
    -- For odd d: there are (d^2-1)/2 X-stabs and (d^2-1)/2 Z-stabs.
    -- For even d: there are d(d-1)/2 X-stabs and d(d-1)/2 Z-stabs,
    --   but the count differs slightly.
    --
    -- A robust construction: use the PRIMAL/DUAL lattice formulation.
    --
    -- Primal lattice (for Z stabilizers):
    --   Horizontal edges in rows 0..d-1, vertical edges in columns 0..d-2
    --   Star operator at each internal vertex of the dual
    --
    -- Actually, let's use the simplest correct construction.
    -- Edges-on-grid formulation:
    --
    -- n = d^2 data qubits on a d x d grid.
    -- X stabilizers: for each cell (i,j) with i in [0,d-2], j in [0,d-2],
    --   where (i+j) is EVEN, the plaquette [(i,j),(i,j+1),(i+1,j),(i+1,j+1)].
    --   Plus boundary weight-2 terms.
    -- Z stabilizers: same but where (i+j) is ODD, plus boundaries.
    --
    -- The boundary terms are critical for k=1:
    -- - Top row (i=0): weight-2 checks along pairs in columns
    -- - Bottom row (i=d-1): weight-2 checks along pairs in columns
    -- - Left column (j=0): weight-2 checks along pairs in rows
    -- - Right column (j=d-1): weight-2 checks along pairs in rows
    --
    -- Convention (Fowler et al.): X boundaries on top/bottom,
    -- Z boundaries on left/right.

    -- Interior weight-4 plaquettes
    plaquette i j = [(i,j), (i,j+1), (i+1,j), (i+1,j+1)]

    -- X-type stabilizers:
    -- Interior: plaquettes at (i,j) with (i+j) even, 0<=i<d-1, 0<=j<d-1
    -- Top boundary: weight-2 at (0,j),(0,j+1) for j where the plaquette
    --   would be X-type if it existed above row 0
    -- Bottom boundary: weight-2 at (d-1,j),(d-1,j+1) similarly
    xStabs = xInterior ++ xTop ++ xBottom

    xInterior = [ plaquette i j
                | i <- [0 .. d - 2]
                , j <- [0 .. d - 2]
                , even (i + j)
                ]

    -- Top boundary: X-type half-plaquettes above the grid
    -- These correspond to where a plaquette at (-1, j) would be,
    -- which has parity ((-1)+j) = j-1. For X-type, j-1 must be even => j odd.
    -- The half-plaquette touches (0,j) and (0,j+1).
    xTop = [ [(0, j), (0, j+1)]
           | j <- [0 .. d - 2]
           , odd j
           ]

    -- Bottom boundary: plaquette at (d-1, j) would be X-type if (d-1+j) even.
    -- The half-plaquette touches (d-1,j) and (d-1,j+1).
    xBottom = [ [(d-1, j), (d-1, j+1)]
              | j <- [0 .. d - 2]
              , even (d - 1 + j)
              ]

    -- Z-type stabilizers:
    -- Interior: plaquettes at (i,j) with (i+j) odd
    -- Left boundary: weight-2 at (i,0),(i+1,0)
    -- Right boundary: weight-2 at (i,d-1),(i+1,d-1)
    zStabs = zInterior ++ zLeft ++ zRight

    zInterior = [ plaquette i j
                | i <- [0 .. d - 2]
                , j <- [0 .. d - 2]
                , odd (i + j)
                ]

    -- Left boundary: plaquette at (i,-1) would have parity (i+(-1)) = i-1.
    -- For Z-type, i-1 must be odd => i even.
    zLeft = [ [(i, 0), (i+1, 0)]
            | i <- [0 .. d - 2]
            , even i
            ]

    -- Right boundary: plaquette at (i, d-1) would have parity (i+d-1).
    -- For Z-type, (i+d-1) must be odd.
    zRight = [ [(i, d-1), (i+1, d-1)]
             | i <- [0 .. d - 2]
             , odd (i + d - 1)
             ]

    hx | null xStabs = bmZero 0 n
       | otherwise   = bmFromRows (map makeStab xStabs)

    hz | null zStabs = bmZero 0 n
       | otherwise   = bmFromRows (map makeStab zStabs)
