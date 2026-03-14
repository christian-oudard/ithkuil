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
  , geminateCa
  , degeminateCa
  , isGeminateCa
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
-- From morphology v1.3.1 Sec 3.6 table (page 14)
-- Simple substitutions: always apply
simpleSubstitutions :: [(Text, Text)]
simpleSubstitutions =
  [ ("pp", "mp"), ("tt", "nt"), ("kk", "nk")
  , ("pb", "mb"), ("kg", "ng")
  , ("ll", "pļ"), ("rr", "ns")
  , ("çy", "nd"), ("řř", "nš")
  ]

-- | Context-dependent substitutions: [C]X → [C]Y
-- Only apply when the pattern occurs after at least one consonant (not word-initial)
contextSubstitutions :: [(Text, Text)]
contextSubstitutions =
  [ ("gm", "x"), ("gn", "ň")
  , ("bm", "v"), ("bn", "ḑ")
  , ("çx", "xw")  -- [C]çx → [C]xw
  ]

-- | Second-pass substitutions for multi-step derivations
-- Applied after context substitutions produce intermediate forms
-- fbm → [C]bm→[C]v → fv → vw
-- ţbn → [C]bn→[C]ḑ → tḑ → ḑy
secondPassSubstitutions :: [(Text, Text)]
secondPassSubstitutions =
  [ ("fv", "vw"), ("tḑ", "ḑy")
  ]

-- | Apply all substitution rules in order:
-- 1. Simple substitutions (apply everywhere)
-- 2. Context-dependent substitutions (only non-initial)
-- 3. Second-pass for multi-step derivations
applySubstitutions :: Text -> Text
applySubstitutions = applySimple secondPassSubstitutions
                   . applyContext contextSubstitutions
                   . applySimple simpleSubstitutions
  where
    applySimple [] t = t
    applySimple ((from, to):rest) t = applySimple rest (T.replace from to t)
    -- Replace pattern only when it occurs after at least one character
    applyContext [] t = t
    applyContext ((from, to):rest) t = applyContext rest (replaceNonInitial from to t)

-- | Replace occurrences of a pattern in text, but only when not at position 0
replaceNonInitial :: Text -> Text -> Text -> Text
replaceNonInitial from to t
  | T.length t < T.length from = t
  | otherwise =
    let len = T.length from
        -- Keep the first character, then do replacements in the rest
        -- But we need to check at each position after index 0
        go i
          | i + len > T.length t = t  -- no more room for pattern
          | T.take len (T.drop i t) == from && i > 0 =
              T.take i t <> to <> replaceNonInitial from to (T.drop (i + len) t)
          | otherwise = go (i + 1)
    in go 0

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

--------------------------------------------------------------------------------
-- Ca Gemination (Sec 3.6.1)
--------------------------------------------------------------------------------

-- | Geminate a Ca consonant cluster (Sec 3.6.1 rules).
-- Uses pre-generated map for lookup; falls back to algorithmic rules.
geminateCa :: Text -> Text
geminateCa ca = Map.findWithDefault (geminateCaForm ca) ca caGemForward

-- | Degeminate a consonant cluster back to its Ca form.
-- Returns the input unchanged if not a recognized geminated form.
degeminateCa :: Text -> Text
degeminateCa t = Map.findWithDefault t t caGemReverse

-- | Check if a consonant cluster is a geminated Ca form.
isGeminateCa :: Text -> Bool
isGeminateCa = flip Map.member caGemReverse

-- | Forward gemination map: Ca form → geminated form
caGemForward :: Map Text Text
caGemForward = Map.fromList
  [(ca, geminateCaForm ca) | ca <- Map.keys caReverse]

-- | Reverse gemination map: geminated form → Ca form
caGemReverse :: Map Text Text
caGemReverse = Map.fromList
  [(geminateCaForm ca, ca) | ca <- Map.keys caReverse]

-- Character classification for gemination rules

isCaStop :: Char -> Bool
isCaStop c = c `elem` ("tkpdgb" :: [Char])

isVoicelessStop :: Char -> Bool
isVoicelessStop c = c `elem` ("tkp" :: [Char])

isLiquidApprox :: Char -> Bool
isLiquidApprox c = c `elem` ("lrřwy" :: [Char])

isSibilant :: Char -> Bool
isSibilant c = c `elem` ("sšzžçcč" :: [Char])

isNonSibFric :: Char -> Bool
isNonSibFric c = c `elem` ("fţvḑ" :: [Char])

isCaNasal :: Char -> Bool
isCaNasal c = c `elem` ("nmň" :: [Char])

-- | Fricatives for rule 6 (voiceless stop + fricative)
isFricR6 :: Char -> Bool
isFricR6 c = c `elem` ("sšfţç" :: [Char])

isLRŘ :: Char -> Bool
isLRŘ c = c `elem` ("lrř" :: [Char])

-- | Find index of first sibilant in text
findFirstSibilant :: Text -> Maybe Int
findFirstSibilant = go 0 . T.unpack
  where
    go _ [] = Nothing
    go i (c:cs)
      | isSibilant c = Just i
      | otherwise = go (i+1) cs

-- | Duplicate the character at position i: "kst" at 1 → "ksst"
gemCharAt :: Int -> Text -> Text
gemCharAt i t = T.take (i+1) t <> T.drop i t

-- | Rule 7: Two stops at end → special substitution
gemStopStop :: [(Text, Text)]
gemStopStop =
  [ ("pt", "bbḑ"), ("pk", "bbv")
  , ("kt", "ggḑ"), ("kp", "ggv")
  , ("tk", "ḑvv"), ("tp", "ddv")
  ]

-- | Rule 8: Stop + nasal at end → special substitution
gemStopNasal :: [(Text, Text)]
gemStopNasal =
  -- 8a: voiceless stop + nasal
  [ ("pm", "vvm"), ("pn", "vvn")
  , ("km", "xxm"), ("kn", "xxn")
  , ("tm", "ḑḑm"), ("tn", "ḑḑn")
  -- 8b: voiced stop + nasal
  , ("bm", "mmw"), ("bn", "mml")
  , ("gm", "ňňw"), ("gn", "ňňl")
  , ("dm", "nnw"), ("dn", "nnl")
  ]

-- | Apply gemination rules 1-8 (without rule 9 l/r/ř prefix handling)
geminateCaInner :: Text -> Text
geminateCaInner t
  | T.null t = t
  | T.length t == 1 = t <> t                              -- Rule 1
  | t == "tļ" = "ttļ"                                      -- Rule 2
  | isCaStop c1 && isLiquidApprox c2 = T.cons c1 t        -- Rule 3
  | Just i <- findFirstSibilant t = gemCharAt i t          -- Rule 4
  | isNonSibFric c1 || isCaNasal c1 = T.cons c1 t         -- Rule 5
  | isVoicelessStop c1 && isFricR6 c2 = gemCharAt 1 t     -- Rule 6
  | Just gem <- lookup t gemStopStop = gem                 -- Rule 7
  | Just gem <- lookup t gemStopNasal = gem                -- Rule 8
  | otherwise = T.cons c1 t                                -- Fallback
  where
    c1 = T.head t
    c2 = T.index t 1

-- | Apply gemination to a Ca form (Sec 3.6.1, rules 1-9)
geminateCaForm :: Text -> Text
geminateCaForm t
  | T.null t = t
  | T.length t == 1 = t <> t                              -- Rule 1
  -- Rule 9: l-/r-/ř- prefix → apply rules to rest
  | isLRŘ c1 && T.length t > 1 =
      let inner = T.tail t
          gemInner = geminateCaInner inner
      in if "çç" `T.isInfixOf` gemInner || "ļļ" `T.isInfixOf` gemInner
         then T.cons c1 (T.cons c1 (T.tail t))  -- fallback: geminate prefix
         else T.cons c1 gemInner
  -- Rules 1-8
  | otherwise = geminateCaInner t
  where c1 = T.head t
