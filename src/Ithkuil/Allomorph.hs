{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Ca Complex: Construction, Parsing, and Allomorphic Rules
--
-- The Ca complex (Slot VI) encodes Configuration, Extension, Affiliation,
-- Perspective, and Essence as a single consonant cluster. It is built
-- compositionally from 4 component tables (Ca1-Ca4), then allomorphic
-- substitution rules are applied.
--
-- For parsing, all 3840 possible Ca forms are pre-generated and stored
-- in a reverse lookup map.
module Ithkuil.Allomorph
  ( constructCa
  , parseCaSlot
  , renderCa
  , caForward
  , caReverse
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Ithkuil.Grammar

--------------------------------------------------------------------------------
-- Component Tables
--------------------------------------------------------------------------------

-- | Ca1: Configuration consonant
ca1 :: Configuration -> Text
ca1 UNI = ""
ca1 DPX = "s"
ca1 DSS = "rt"
ca1 DSC = "rk"
ca1 DSF = "rp"
ca1 DDS = "rn"
ca1 DDC = "rň"
ca1 DDF = "rm"
ca1 DFS = "řt"
ca1 DFC = "řk"
ca1 DFF = "řp"
ca1 MSS = "t"
ca1 MSC = "k"
ca1 MSF = "p"
ca1 MDS = "n"
ca1 MDC = "ň"
ca1 MDF = "m"
ca1 MFS = "lt"
ca1 MFC = "lk"
ca1 MFF = "lp"

-- | Ca2: Extension consonant (non-UNIPLEX form)
-- For UNIPLEX Configuration, see constructCaRaw which uses ca2Standalone
ca2 :: Extension -> Text
ca2 DEL = ""
ca2 PRX = "s"
ca2 ICP = "š"
ca2 ATV = "f"
ca2 GRA = "ţ"
ca2 DPL = "ç"

-- | Ca2 standalone: Extension consonant for UNIPLEX Configuration
-- These are voiced forms used when Config is UNI (no Ca1 consonant)
ca2Standalone :: Extension -> Text
ca2Standalone DEL = ""
ca2Standalone PRX = "d"
ca2Standalone ICP = "g"
ca2Standalone ATV = "b"
ca2Standalone GRA = "gz"
ca2Standalone DPL = "bz"

-- | Ca3: Affiliation consonant
-- (standalone form, after-consonant form)
-- Standalone = when Ca1 and Ca2 are both empty
-- After-consonant = when Ca1 or Ca2 is present
ca3 :: Affiliation -> (Text, Text)
ca3 CSL = ("", "")
ca3 ASO = ("d", "t")
ca3 COA = ("g", "k")
ca3 VAR = ("b", "p")

-- | Ca3 standalone: Affiliation when used alone (UNI/DEL/M/NRM + affiliation)
ca3Standalone :: Affiliation -> Text
ca3Standalone CSL = ""
ca3Standalone ASO = "nļ"
ca3Standalone COA = "rļ"
ca3Standalone VAR = "ň"

-- | Ca4: Perspective + Essence consonant
-- (standalone form, after-consonant form)
-- Standalone = when Ca1, Ca2, Ca3 are all empty
-- After-consonant = when any of Ca1, Ca2, Ca3 is present
ca4 :: Perspective -> Essence -> (Text, Text)
ca4 M_ NRM = ("l", "")
ca4 G_ NRM = ("r", "r")
ca4 N_ NRM = ("v", "w")
ca4 A_ NRM = ("z", "y")
ca4 M_ RPV = ("ř", "ř")
ca4 G_ RPV = ("tļ", "l")
ca4 N_ RPV = ("lm", "m")
ca4 A_ RPV = ("ln", "n")

--------------------------------------------------------------------------------
-- Ca Construction
--------------------------------------------------------------------------------

-- | Construct raw Ca from components (before allomorphic substitutions)
-- Three modes:
-- 1. UNIPLEX (Ca1="") with Extension → use ca2Standalone for voiced forms
-- 2. UNIPLEX with Affiliation only → use ca3Standalone
-- 3. Non-UNIPLEX (Ca1≠"") → use compositional ca2/ca3/ca4 tables
constructCaRaw :: SlotVI -> Text
constructCaRaw (co, af, pe, ex, es)
  -- UNIPLEX with Extension (voiced forms): d, g, b, gz, bz
  | co == UNI && ex /= DEL =
      ca2Standalone ex <> ca4suffix pe es
  -- UNIPLEX with only Affiliation (standalone forms): nļ, rļ, ň
  | co == UNI && af /= CSL =
      ca3Standalone af <> ca4suffix pe es
  -- Fully standalone (UNI/CSL/DEL) → perspective standalone form
  | co == UNI =
      fst (ca4 pe es)
  -- Non-UNIPLEX: compositional
  | otherwise = c1 <> c2 <> c3' <> c4''
  where
    c1 = ca1 co
    c2 = ca2 ex
    c3' = snd (ca3 af)  -- always after-consonant when Ca1 is present
    -- After Configuration, M_/NRM adds "l"; other perspectives use their form
    c4c = case (pe, es) of
      (M_, NRM) -> "l"
      _         -> snd (ca4 pe es)
    -- Special combination rules (from mamkait)
    c4''
      | hasŘPrefix c1 && c4c == "r" = "v"
      | not (T.null c2) && not (T.null c3') && c4c == "m" = "h"
      | not (T.null c2) && not (T.null c3') && c4c == "n" = "ç"
      | otherwise = c4c
    hasŘPrefix t = T.take 1 t == "ř"
    -- Ca4 suffix for UNIPLEX with Extension/Affiliation
    ca4suffix p e = snd (ca4 p e)

-- | Construct Ca with allomorphic substitutions applied
constructCa :: SlotVI -> Text
constructCa = applySubstitutions . constructCaRaw

--------------------------------------------------------------------------------
-- Allomorphic Substitutions
--------------------------------------------------------------------------------

-- | All allomorphic substitution rules
-- Applied sequentially to the raw Ca consonant cluster
substitutions :: [(Text, Text)]
substitutions =
  [ ("ts", "c")
  , ("tš", "č")
  , ("tţ", "ḑ")
  , ("np", "mv")
  , ("ňk", "ňz")
  -- nf → v only when not word-final (simplified: always apply in Ca)
  , ("nf", "v")
  , ("tf", "fs")
  , ("kf", "fš")
  , ("ňy", "ňž")
  , ("çy", "ž")
  , ("cy", "j")
  , ("čy", "dž")
  , ("nn", "nz")
  , ("mm", "mz")
  , ("ltt", "ld")
  , ("lkk", "lg")
  , ("lpp", "lb")
  , ("rnm", "nž")
  , ("rmn", "mž")
  , ("rtt", "rd")
  , ("rkk", "rg")
  , ("rpp", "rb")
  , ("rňm", "ňv")
  , ("rňn", "nḑ")
  , ("řtt", "řd")
  , ("řkk", "řg")
  , ("řpp", "řb")
  ]

-- | Apply all substitution rules sequentially
applySubstitutions :: Text -> Text
applySubstitutions = applyAll substitutions
  where
    applyAll [] t = t
    applyAll ((from, to):rest) t = applyAll rest (T.replace from to t)

--------------------------------------------------------------------------------
-- Pre-generated Lookup Tables
--------------------------------------------------------------------------------

-- | All possible SlotVI values
allSlotVI :: [SlotVI]
allSlotVI = (,,,,) <$> allOf <*> allOf <*> allOf <*> allOf <*> allOf

-- | Forward map: SlotVI -> Ca consonant cluster
caForward :: Map SlotVI Text
caForward = Map.fromList [(s, constructCa s) | s <- allSlotVI]

-- | Reverse map: Ca consonant cluster -> SlotVI
-- When multiple SlotVI produce the same Ca form, the first one wins
caReverse :: Map Text SlotVI
caReverse = Map.fromList [(constructCa s, s) | s <- reverse allSlotVI]

-- | Parse a Ca consonant cluster to SlotVI
parseCaSlot :: Text -> Maybe SlotVI
parseCaSlot = flip Map.lookup caReverse

-- | Render a SlotVI to Ca consonant cluster
renderCa :: SlotVI -> Text
renderCa s = Map.findWithDefault (constructCa s) s caForward
