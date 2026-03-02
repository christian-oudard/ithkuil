{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Script (Writing System)
-- Morpho-phonemic script with 4 character types
module Ithkuil.Script where

import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Character Types
--------------------------------------------------------------------------------

-- | The four character types in Ithkuil script
data CharType
  = Primary      -- ^ Consonant roots (Cr) - main semantic content
  | Secondary    -- ^ Other consonants (Ca, Cs, etc.)
  | Tertiary     -- ^ Vowels (Vv, Vr, Vc, etc.)
  | Quaternary   -- ^ Diacritics and modifiers
  deriving (Show, Eq, Ord, Bounded, Enum)

--------------------------------------------------------------------------------
-- Primary Characters (Consonant Roots)
--------------------------------------------------------------------------------

-- | Primary character with core and extensions
data PrimaryChar = PrimaryChar
  { pcCore :: CoreShape       -- Main consonant shape
  , pcTop :: Maybe TopExt     -- Top extension (additional consonant)
  , pcBottom :: Maybe BotExt  -- Bottom extension (additional consonant)
  }
  deriving (Show, Eq)

-- | Core shapes for primary characters
-- Based on phonetic features of the first consonant
data CoreShape
  = CoreP | CoreB | CoreT | CoreD | CoreK | CoreG
  | CoreF | CoreV | CoreS | CoreZ | CoreŠ | CoreŽ
  | CoreŢ | CoreḐ | CoreÇ | CoreX | CoreH
  | CoreC | CoreJ | CoreČ | CoreDŽ
  | CoreM | CoreN | CoreŇ
  | CoreL | CoreR | CoreŘ | CoreĻ
  | CoreW | CoreY
  | CoreGlottal
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Get core shape from consonant
coreFromConsonant :: Text -> Maybe CoreShape
coreFromConsonant "p"  = Just CoreP
coreFromConsonant "b"  = Just CoreB
coreFromConsonant "t"  = Just CoreT
coreFromConsonant "d"  = Just CoreD
coreFromConsonant "k"  = Just CoreK
coreFromConsonant "g"  = Just CoreG
coreFromConsonant "f"  = Just CoreF
coreFromConsonant "v"  = Just CoreV
coreFromConsonant "s"  = Just CoreS
coreFromConsonant "z"  = Just CoreZ
coreFromConsonant "š"  = Just CoreŠ
coreFromConsonant "ž"  = Just CoreŽ
coreFromConsonant "ţ"  = Just CoreŢ
coreFromConsonant "ḑ"  = Just CoreḐ
coreFromConsonant "ç"  = Just CoreÇ
coreFromConsonant "x"  = Just CoreX
coreFromConsonant "h"  = Just CoreH
coreFromConsonant "c"  = Just CoreC
coreFromConsonant "ẓ"  = Just CoreJ
coreFromConsonant "č"  = Just CoreČ
coreFromConsonant "j"  = Just CoreDŽ
coreFromConsonant "m"  = Just CoreM
coreFromConsonant "n"  = Just CoreN
coreFromConsonant "ň"  = Just CoreŇ
coreFromConsonant "l"  = Just CoreL
coreFromConsonant "r"  = Just CoreR
coreFromConsonant "ř"  = Just CoreŘ
coreFromConsonant "ļ"  = Just CoreĻ
coreFromConsonant "w"  = Just CoreW
coreFromConsonant "y"  = Just CoreY
coreFromConsonant "'"  = Just CoreGlottal
coreFromConsonant _    = Nothing

-- | Top extension shapes
data TopExt
  = TopNone | TopBar | TopCurve | TopHook
  | TopDot | TopDoubleDot | TopLine
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Bottom extension shapes
data BotExt
  = BotNone | BotTail | BotCurve | BotHook
  | BotDot | BotDoubleDot | BotLine
  deriving (Show, Eq, Ord, Bounded, Enum)

--------------------------------------------------------------------------------
-- Secondary Characters
--------------------------------------------------------------------------------

-- | Secondary character for non-root consonants
data SecondaryChar = SecondaryChar
  { scBase :: SecondaryBase
  , scDiacritics :: [Diacritic]
  }
  deriving (Show, Eq)

data SecondaryBase
  = SecBaseStop
  | SecBaseFric
  | SecBaseAffric
  | SecBaseNasal
  | SecBaseLiquid
  | SecBaseApprox
  deriving (Show, Eq, Ord, Bounded, Enum)

--------------------------------------------------------------------------------
-- Tertiary Characters (Vowels)
--------------------------------------------------------------------------------

-- | Tertiary character for vowels
data TertiaryChar = TertiaryChar
  { tcBase :: VowelBase
  , tcRegister :: VowelRegister
  }
  deriving (Show, Eq)

-- | Base vowel shapes (9 vowels)
data VowelBase
  = VowelA | VowelÄ | VowelE | VowelË | VowelI
  | VowelÖ | VowelO | VowelÜ | VowelU
  deriving (Show, Eq, Ord, Bounded, Enum)

vowelBaseFromChar :: Text -> Maybe VowelBase
vowelBaseFromChar "a" = Just VowelA
vowelBaseFromChar "ä" = Just VowelÄ
vowelBaseFromChar "e" = Just VowelE
vowelBaseFromChar "ë" = Just VowelË
vowelBaseFromChar "i" = Just VowelI
vowelBaseFromChar "ö" = Just VowelÖ
vowelBaseFromChar "o" = Just VowelO
vowelBaseFromChar "ü" = Just VowelÜ
vowelBaseFromChar "u" = Just VowelU
vowelBaseFromChar _   = Nothing

-- | Vowel register (determines vertical placement)
data VowelRegister
  = RegisterTop    -- ^ Top of staff
  | RegisterMid    -- ^ Middle of staff
  | RegisterBot    -- ^ Bottom of staff
  deriving (Show, Eq, Ord, Bounded, Enum)

--------------------------------------------------------------------------------
-- Quaternary Characters (Diacritics)
--------------------------------------------------------------------------------

-- | Diacritic marks
data Diacritic
  = DiacStress      -- ^ Stress mark
  | DiacGeminate    -- ^ Gemination mark
  | DiacSyllabic    -- ^ Syllabic consonant
  | DiacGlottal     -- ^ Glottal insertion
  | DiacTone1       -- ^ Tone mark 1
  | DiacTone2       -- ^ Tone mark 2
  deriving (Show, Eq, Ord, Bounded, Enum)

--------------------------------------------------------------------------------
-- Script Rendering (to SVG paths - conceptual)
--------------------------------------------------------------------------------

-- | Glyph data for rendering
data Glyph = Glyph
  { glyphType :: CharType
  , glyphPath :: Text       -- SVG path data (placeholder)
  , glyphWidth :: Double
  , glyphHeight :: Double
  }
  deriving (Show, Eq)

-- | Convert formative to sequence of glyphs
-- This is a placeholder; full implementation would generate actual paths
formativeToGlyphs :: Text -> [Glyph]
formativeToGlyphs _ = []  -- TODO: Implement

--------------------------------------------------------------------------------
-- Unicode Block (Proposed)
--------------------------------------------------------------------------------

-- | Proposed Unicode code point range for Ithkuil script
-- (Not yet in Unicode; using Private Use Area)
unicodePUAStart :: Int
unicodePUAStart = 0xE000  -- Private Use Area start

-- | Convert script character to PUA code point
charToUnicodePUA :: CharType -> Int -> Char
charToUnicodePUA ctype idx =
  let offset = case ctype of
        Primary    -> 0
        Secondary  -> 256
        Tertiary   -> 512
        Quaternary -> 768
  in toEnum (unicodePUAStart + offset + idx)

--------------------------------------------------------------------------------
-- Writing Direction
--------------------------------------------------------------------------------

-- | Ithkuil is written left-to-right, top-to-bottom
-- Characters are placed on a "staff" like musical notation
data StaffPosition = StaffPosition
  { spX :: Double        -- Horizontal position
  , spY :: Double        -- Vertical position (on staff)
  , spRegister :: Int    -- Staff line (1-5)
  }
  deriving (Show, Eq)
