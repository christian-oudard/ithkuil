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

-- | Ca1: Configuration consonant (from grammar ch03 table)
ca1 :: Configuration -> Text
ca1 UNI = ""
ca1 DPX = "s"
ca1 DSS = "c"
ca1 DSC = "ks"
ca1 DSF = "ps"
ca1 DDS = "ţs"
ca1 DDC = "fs"
ca1 DDF = "š"
ca1 DFS = "č"
ca1 DFC = "kš"
ca1 DFF = "pš"
ca1 MSS = "t"
ca1 MSC = "k"
ca1 MSF = "p"
ca1 MDS = "ţ"
ca1 MDC = "f"
ca1 MDF = "ç"
ca1 MFS = "z"
ca1 MFC = "ž"
ca1 MFF = "ẓ"

-- | Ca2: Extension consonant (non-UNIPLEX, voiceless form)
-- Placed between Configuration and Perspective in the Ca complex
ca2 :: Extension -> Text
ca2 DEL = ""
ca2 PRX = "t"
ca2 ICP = "k"
ca2 ATV = "p"
ca2 GRA = "g"
ca2 DPL = "b"

-- | Ca2 standalone: Extension consonant for UNIPLEX Configuration
-- These are voiced forms used when Config is UNI (no Ca1 consonant)
ca2Standalone :: Extension -> Text
ca2Standalone DEL = ""
ca2Standalone PRX = "d"
ca2Standalone ICP = "g"
ca2Standalone ATV = "b"
ca2Standalone GRA = "gz"
ca2Standalone DPL = "bz"

-- | Ca3: Affiliation prefix
-- (standalone form, before-consonant form)
-- Standalone = UNI/DEL with no other components present
-- Before-consonant = prefix before Configuration consonant
ca3 :: Affiliation -> Text
ca3 CSL = ""
ca3 ASO = "l"
ca3 COA = "r"
ca3 VAR = "ř"

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
ca4 A_ NRM = ("j", "y")
ca4 M_ RPV = ("tļ", "l")
ca4 G_ RPV = ("ř", "ř")
ca4 N_ RPV = ("m", "m")
ca4 A_ RPV = ("n", "n")

--------------------------------------------------------------------------------
-- Ca Construction
--------------------------------------------------------------------------------

-- | Construct raw Ca from components (before allomorphic substitutions)
-- Grammar order: Affiliation + Configuration + Extension + Perspective/Essence
-- Special cases:
-- 1. UNIPLEX with Extension → use ca2Standalone (voiced forms: d, g, b, gz, bz)
-- 2. UNIPLEX with Affiliation only → use ca3Standalone (nļ, rļ, ň)
-- 3. Fully standalone (UNI/CSL/DEL/M_/NRM) → standalone perspective form
-- 4. General: Aff-prefix + Config + Extension + Perspective suffix
constructCaRaw :: SlotVI -> Text
constructCaRaw (co, af, pe, ex, es)
  -- UNIPLEX with Extension (voiced standalone forms)
  | co == UNI && ex /= DEL =
      ca2Standalone ex <> perspSuffix pe es
  -- UNIPLEX with only Affiliation (standalone forms)
  | co == UNI && af /= CSL =
      ca3Standalone af <> perspSuffix pe es
  -- Fully standalone (UNI/CSL/DEL) → perspective standalone form
  | co == UNI =
      fst (ca4 pe es)
  -- General: Affiliation prefix + Configuration + Extension + Perspective
  | otherwise = affPfx <> cfg <> ext <> persp''
  where
    affPfx = ca3 af
    cfg = ca1 co
    ext = ca2 ex
    -- Perspective + Essence (after-consonant form since config is present)
    persp = snd (ca4 pe es)
    -- Special combination rules from grammar section 3.5.1:
    -- N_ RPV (m) → h when preceded by [C]t, [C]k, or [C]p
    -- A_ RPV (n) → ç when preceded by [C]t, [C]k, or [C]p
    persp''
      | persp == "m" && endsWithStop = "h"
      | persp == "n" && endsWithStop = "ç"
      | otherwise = persp
    preceding = affPfx <> cfg <> ext
    endsWithStop = case T.unsnoc preceding of
      Just (_, c) -> c `elem` ("tkp" :: [Char])
      Nothing -> False
    -- Perspective suffix for UNIPLEX with Extension/Affiliation
    perspSuffix p e = snd (ca4 p e)

-- | Construct Ca with allomorphic substitutions applied
constructCa :: SlotVI -> Text
constructCa = applySubstitutions . constructCaRaw

--------------------------------------------------------------------------------
-- Allomorphic Substitutions
--------------------------------------------------------------------------------

-- | Allomorphic substitution rules for Ca consonant clusters
-- From grammar ch03 section 3.6 table
substitutions :: [(Text, Text)]
substitutions =
  [ ("pp", "mp")  -- MSF + ATV, or other pp clusters
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
