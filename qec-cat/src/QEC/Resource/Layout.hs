-- | Physical qubit layout computation.
--
-- Code-family-aware layout: each code family has its own formula for
-- data, syndrome, and routing qubits.
module QEC.Resource.Layout
  ( CodeFamily(..)
  , dataQubits
  , syndromeQubits
  , routingQubits
  ) where

-- Re-export CodeFamily so callers don't need to import Resource directly
-- for layout-only use.  The canonical definition lives in QEC.Resource;
-- here we accept it as a parameter.

-- NOTE: CodeFamily is defined in QEC.Resource to avoid circular imports.
-- Layout functions take it as a parameter from the caller.

-- | Dummy re-declaration removed — we import CodeFamily from the caller
-- via the pipeline.  Layout functions are polymorphic over a tag.

-- We use a simple ADT mirroring QEC.Resource.CodeFamily to avoid
-- circular module dependencies.

-- | Code family tag for layout calculations.
data CodeFamily
  = RepetitionCat    -- ^ Repetition code (phase-flip only, cat qubit)
  | SurfaceCode      -- ^ Standard surface code
  | LDPCCat          -- ^ LDPC code tailored for cat qubits
  deriving stock (Show, Eq)

-- | Number of data qubits.
--
-- * RepetitionCat: @nLogical * d@ (d data qubits per logical qubit)
-- * SurfaceCode:   @nLogical * d^2@ (d×d data patch per logical qubit)
-- * LDPCCat:       @nLogical * (n\/k)@ where n\/k = 4 for the base code
dataQubits :: CodeFamily -> Int -> Int -> Int
dataQubits RepetitionCat nLogical d = nLogical * d
dataQubits SurfaceCode   nLogical d = nLogical * d * d
dataQubits LDPCCat       nLogical _ = nLogical * 4  -- n/k = 136/34 = 4

-- | Number of syndrome extraction qubits.
--
-- * RepetitionCat: @nLogical * (d - 1)@ (one ancilla per check)
-- * SurfaceCode:   @nLogical * d^2@ (equal to data qubits)
-- * LDPCCat:       @nLogical * (m\/k)@ where m\/k = 3 (102 checks / 34 logical)
syndromeQubits :: CodeFamily -> Int -> Int -> Int
syndromeQubits RepetitionCat nLogical d = nLogical * (d - 1)
syndromeQubits SurfaceCode   nLogical d = nLogical * d * d
syndromeQubits LDPCCat       nLogical _ = nLogical * 3  -- m/k = 102/34 = 3

-- | Number of routing qubits for inter-logical-qubit operations.
--
-- * RepetitionCat: @nLogical@ (1 per logical qubit)
-- * SurfaceCode:   @nLogical * 2@ (lattice surgery channels)
-- * LDPCCat:       @nLogical@ (1 per logical qubit, same as rep-cat)
routingQubits :: CodeFamily -> Int -> Int
routingQubits RepetitionCat nLogical = nLogical
routingQubits SurfaceCode   nLogical = nLogical * 2
routingQubits LDPCCat       nLogical = nLogical
