{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil III (2011) Grammar Types
-- V3 has a similar structure to V4 but with some different categories:
-- - Designation (instead of Effect)
-- - Sanction (instead of Validation)
-- - Pattern (instead of Specification)
module Ithkuil.V3.Grammar
  ( -- * V3-Specific Categories
    Designation(..)
  , Pattern(..)
  , Sanction(..)
    -- * V3 Formative Structure
  , V3Formative(..)
  , V3SlotII
  , V3SlotIV
  , V3Ca
    -- * Re-exports from V4 that are shared
  , Stem(..)
  , Version(..)
  , Function(..)
  , Context(..)
  , Configuration(..)
  , Affiliation(..)
  , Perspective(..)
  , Extension(..)
  , Essence(..)
  , Case(..)
  , Root(..)
  ) where

import Data.Text (Text)
import Ithkuil.Grammar
  ( Stem(..), Version(..), Function(..), Context(..)
  , Configuration(..), Affiliation(..), Perspective(..)
  , Extension(..), Essence(..), Case(..), Root(..)
  )

--------------------------------------------------------------------------------
-- V3-Specific Categories
--------------------------------------------------------------------------------

-- | Designation (V3) - Formal vs Informal register
-- This was incorporated differently in V4
data Designation
  = Formal    -- ^ FML - formal/ceremonial usage
  | Informal  -- ^ IFL - casual/colloquial usage
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Pattern (V3) - How the stem meanings relate to each other
-- This evolved into Specification in V4
data Pattern
  = P1  -- ^ Pattern 1: holistic/generic
  | P2  -- ^ Pattern 2: specific aspect A
  | P3  -- ^ Pattern 3: specific aspect B
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Sanction (V3) - Evidentiality/epistemic status
-- This became Validation in V4
data Sanction
  = PPS  -- ^ Propositional: neutral assertion
  | EPI  -- ^ Epistemological: known truth
  | ALG  -- ^ Allegorical: metaphorical truth
  | IPU  -- ^ Imputative: assumed/presupposed
  | RFU  -- ^ Refutative: denying/refuting
  | REB  -- ^ Rebuttative: counter-asserting
  | CJT  -- ^ Conjectural: theoretical
  | EXV  -- ^ Expatiative: elaborating
  | AXM  -- ^ Axiomatic: self-evident truth
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- V3 Slot Types
--------------------------------------------------------------------------------

-- | V3 Slot II: Pattern + Stem + Designation
type V3SlotII = (Pattern, Stem, Designation)

-- | V3 Slot IV: Function + Pattern + Context
-- Note: In V3, Function and Context work similarly to V4
type V3SlotIV = (Function, Pattern, Context)

-- | V3 Ca Complex
-- Structure: Configuration + Extension + Affiliation + Perspective + Essence
data V3Ca = V3Ca
  { v3Config      :: Configuration
  , v3Extension   :: Extension
  , v3Affiliation :: Affiliation
  , v3Perspective :: Perspective
  , v3Essence     :: Essence
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- V3 Formative Structure
--------------------------------------------------------------------------------

-- | V3 Formative with all slots
data V3Formative = V3Formative
  { v3fSlotII  :: V3SlotII      -- Pattern + Stem + Designation
  , v3fRoot    :: Root          -- Cr (root consonants)
  , v3fSlotIV  :: V3SlotIV      -- Function + Pattern + Context
  , v3fCa      :: Maybe V3Ca    -- Ca complex
  , v3fCase    :: Maybe Case    -- Vc (case vowel)
  , v3fRaw     :: [Text]        -- Original conjuncts
  } deriving (Show, Eq)

-- | Default V3 Ca values
defaultV3Ca :: V3Ca
defaultV3Ca = V3Ca UNI DEL CSL M_ NRM
