{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V3 (Elartkha) Full Formative Parser
-- Wraps basic parsing with tone detection (Version) and stress detection
-- (Designation + Relation).
--
-- V3 encoding:
--   Tone → Version (6 tones: falling, high, rising, low, falling-rising, rising-falling)
--   Stress → Designation × Relation:
--     Penultimate + Unframed = IFL/UNFRAMED (default)
--     Ultimate + Unframed    = FML/UNFRAMED
--     Penultimate + Framed   = IFL/FRAMED
--     Ultimate + Framed      = FML/FRAMED
module Ithkuil.V3.FullParse
  ( ParseResult(..)
  , parseFormative
  , parseSentence
  , deconstructFormative
  , DeconstructedSlot(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Ithkuil.V3.Grammar
import qualified Ithkuil.V3.Parse as P
import Ithkuil.V3.Lexicon (Lexicon, lookupRoot)

--------------------------------------------------------------------------------
-- Parser Types
--------------------------------------------------------------------------------

data ParseResult a
  = Success a
  | Failure Text
  deriving (Show, Eq)

instance Functor ParseResult where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure e) = Failure e

instance Applicative ParseResult where
  pure = Success
  Success f <*> Success a = Success (f a)
  Failure e <*> _ = Failure e
  _ <*> Failure e = Failure e

instance Monad ParseResult where
  Success a >>= f = f a
  Failure e >>= _ = Failure e

--------------------------------------------------------------------------------
-- Full Parsing
--------------------------------------------------------------------------------

-- | Parse a V3 formative with Ca table, tone, and stress detection.
parseFormative :: P.CaTables -> Text -> ParseResult Formative
parseFormative ca word =
  case P.parseFormativeWithCa ca (T.toLower word) of
    Left err -> Failure (T.pack (show err))
    Right f  ->
      let (desig, rel) = detectStress word
          tone = detectTone word
      in Success f
           { fTone     = tone
           , fDesig    = desig
           , fRelation = rel
           }

-- | Parse a sentence (space-separated words)
parseSentence :: P.CaTables -> Text -> [ParseResult Formative]
parseSentence ca = map (parseFormative ca) . T.words

--------------------------------------------------------------------------------
-- Tone Detection
-- V3 uses tone contour on the first syllable vowel to encode Version.
-- In romanization, tone marks appear as diacritics on vowels.
--------------------------------------------------------------------------------

-- | Detect tone from the first vowel's diacritic
detectTone :: Text -> Tone
detectTone t = case T.find isToneMarked t of
  Just c  -> charToTone c
  Nothing -> Falling  -- Default: falling (unmarked)
  where
    isToneMarked c = c `elem` ("\x0304\x0301\x0332\x030C\x0302" :: [Char])
    charToTone '\x0304' = High           -- macron
    charToTone '\x0301' = Rising         -- acute
    charToTone '\x0332' = Low            -- low line
    charToTone '\x030C' = FallingRising  -- caron
    charToTone '\x0302' = RisingFalling  -- circumflex
    charToTone _        = Falling

--------------------------------------------------------------------------------
-- Stress Detection
-- V3 uses stress position to encode Designation + Relation:
--   Penultimate = IFL (Informal)
--   Ultimate    = FML (Formal)
-- For framed relations, stress shifts to opposite syllable.
-- In romanization, stress is marked with acute accent on the stressed vowel.
--------------------------------------------------------------------------------

-- | Detect Designation and Relation from stress position
detectStress :: Text -> (Designation, Relation)
detectStress t =
  let syllables = countSyllables t
      stressPos = findStressPosition t
  in case stressPos of
       Nothing -> (IFL, Unframed)  -- Default
       Just pos
         | syllables <= 1      -> (IFL, Unframed)
         | pos == syllables    -> (FML, Unframed)  -- Ultimate
         | pos == syllables -1 -> (IFL, Unframed)  -- Penultimate (default)
         | otherwise           -> (IFL, Framed)    -- Antepenultimate

-- | Count syllables (= number of vowel groups)
countSyllables :: Text -> Int
countSyllables = go False 0 . T.unpack
  where
    go _ n [] = n
    go inV n (c:cs)
      | isVow c   = go True (if inV then n else n + 1) cs
      | otherwise  = go False n cs
    isVow c = c `elem` ("aâäeêëiîoôöuûüáéíóúàèìòùæøɨ" :: [Char])

-- | Find which syllable (1-based from start) has stress marking
findStressPosition :: Text -> Maybe Int
findStressPosition t = go False 0 (T.unpack t)
  where
    stressChars = "áéíóú" :: [Char]
    go _ _ [] = Nothing
    go inV n (c:cs)
      | c `elem` stressChars = Just (if inV then n else n + 1)
      | isVow c              = go True (if inV then n else n + 1) cs
      | otherwise            = go False n cs
    isVow c = c `elem` ("aâäeêëiîoôöuûüáéíóúàèìòùæøɨ" :: [Char])

--------------------------------------------------------------------------------
-- Deconstruction (slot-by-slot analysis with meanings)
--------------------------------------------------------------------------------

data DeconstructedSlot = DeconstructedSlot
  { dsSlotName :: Text
  , dsRaw      :: Text
  , dsValues   :: [(Text, Text)]  -- (category, value) pairs
  } deriving (Show, Eq)

-- | Deconstruct a parsed formative into labeled slots with meanings
deconstructFormative :: Lexicon -> Formative -> [DeconstructedSlot]
deconstructFormative lex' f =
  [ DeconstructedSlot "Tone" (T.pack (show (fTone f)))
      [("Version", T.pack (show (toneToVersion (fTone f))))]
  , DeconstructedSlot "Vr" ""
      [ ("Function", T.pack (show func))
      , ("Pattern", T.pack (show pat))
      , ("Stem", T.pack (show stem))
      ]
  , DeconstructedSlot "Cr" cr
      [("Root", maybe "?" id (lookupRoot cr lex'))]
  , DeconstructedSlot "Vc" ""
      [("Case", caseAbbrev (fCase f))]
  , DeconstructedSlot "Ca" ""
      (deconstructCa (fCa f))
  , DeconstructedSlot "Stress" ""
      [ ("Designation", T.pack (show (fDesig f)))
      , ("Relation", T.pack (show (fRelation f)))
      ]
  ]
  where
    (func, pat, stem) = fVr f
    Root cr = fRoot f

deconstructCa :: CaComplex -> [(Text, Text)]
deconstructCa (ess, ext, per, aff, cfg) = filter ((/= "") . snd)
  [ ("Configuration", if cfg /= UNI then T.pack (show cfg) else "")
  , ("Affiliation", if aff /= CSL then T.pack (show aff) else "")
  , ("Perspective", if per /= M_ then showPer per else "")
  , ("Extension", if ext /= DEL then T.pack (show ext) else "")
  , ("Essence", if ess /= NRM then T.pack (show ess) else "")
  ]
  where
    showPer M_ = "M"
    showPer U_ = "U"
    showPer N_ = "N"
    showPer A_ = "A"
