{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil II (Ilaksh, 2007) Grammar Types
-- V2 used tones to reduce the phonemic inventory
module Ithkuil.V2.Grammar
  ( -- * V2-Specific Categories
    Tone(..)
  , V2Vowel(..)
    -- * V2 Formative Structure
  , V2Formative(..)
    -- * Re-exports from V3/V4 that are shared
  , Stem(..)
  , Version(..)
  , Function(..)
  , Context(..)
  , Configuration(..)
  , Case(..)
  , Root(..)
  ) where

import Data.Text (Text)
import Ithkuil.Grammar
  ( Stem(..), Version(..), Function(..), Context(..)
  , Configuration(..), Case(..), Root(..)
  )

--------------------------------------------------------------------------------
-- V2-Specific Categories
--------------------------------------------------------------------------------

-- | Tone markers used in V2 (Ilaksh)
-- V2 relied on tonal distinctions to reduce consonant inventory
data Tone
  = High     -- ^ High tone (acute accent: á)
  | Low      -- ^ Low tone (grave accent: à)
  | Rising   -- ^ Rising tone (caron: ǎ)
  | Falling  -- ^ Falling tone (circumflex: â)
  | Neutral  -- ^ No tone marking
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | V2 vowel with tonal information
data V2Vowel = V2Vowel
  { v2Base :: Char
  , v2Tone :: Tone
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- V2 Formative Structure
--------------------------------------------------------------------------------

-- | V2 Formative
-- Similar to V3 but with tonal vowels
data V2Formative = V2Formative
  { v2fRoot    :: Root          -- Cr (root consonants)
  , v2fStem    :: Stem          -- From vowel pattern
  , v2fVersion :: Version       -- Processual/Completive
  , v2fFunc    :: Function      -- Stative/Dynamic
  , v2fRaw     :: [Text]        -- Original conjuncts
  } deriving (Show, Eq)
