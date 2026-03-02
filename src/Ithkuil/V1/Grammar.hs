{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil I (2004) Grammar Types
-- V1 used consonant grades and biliteral (two-consonant) roots
module Ithkuil.V1.Grammar
  ( -- * V1-Specific Categories
    ConsonantGrade(..)
  , V1Mode(..)
  , V1Root(..)
    -- * V1 Formative Structure
  , V1Formative(..)
    -- * Re-exports
  , Stem(..)
  , Configuration(..)
  ) where

import Data.Text (Text)
import Ithkuil.Grammar (Stem(..), Configuration(..))

--------------------------------------------------------------------------------
-- V1-Specific Categories
--------------------------------------------------------------------------------

-- | Consonant Grade System (V1)
-- V1 used systematic consonant mutations called "grades"
data ConsonantGrade
  = G1  -- ^ Grade 1: base consonant (k)
  | G2  -- ^ Grade 2: geminate (kk)
  | G3  -- ^ Grade 3: aspirate (kh)
  | G4  -- ^ Grade 4: glottalized (k')
  | G5  -- ^ Grade 5: palatalized (ky)
  | G6  -- ^ Grade 6: velarized (kw)
  | G7  -- ^ Grade 7: fricativized (x for k)
  | G8  -- ^ Grade 8: dental addition (tk for k)
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | V1 Mode - Primary vs Secondary
-- Secondary mode added vowel mutation (u -> ui)
data V1Mode
  = Primary    -- ^ Normal/real-world meaning
  | Secondary  -- ^ Imaginary/hypothetical meaning
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | V1 Biliteral Root (like Semitic languages)
-- Roots consist of two consonant radicals
data V1Root = V1Root
  { v1C1 :: Text  -- ^ First radical consonant
  , v1C2 :: Text  -- ^ Second radical consonant
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- V1 Formative Structure
--------------------------------------------------------------------------------

-- | V1 Formative with biliteral root and consonant grades
data V1Formative = V1Formative
  { v1fRoot    :: V1Root          -- Biliteral root (C1-C2)
  , v1fStem    :: Stem            -- From vocalic infix
  , v1fGrade   :: ConsonantGrade  -- Applied to first radical
  , v1fMode    :: V1Mode          -- Primary/Secondary
  , v1fConfig  :: Configuration   -- From grade/prefix
  , v1fRaw     :: [Text]          -- Original conjuncts
  } deriving (Show, Eq)
