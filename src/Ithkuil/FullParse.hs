{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Full Formative Parser
-- Complete 10-slot parsing with stress analysis
module Ithkuil.FullParse
  ( ParseResult(..)
  , parseFormative
  , detectStress
  , countSyllables
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import Ithkuil.Grammar
import Ithkuil.Parse
  ( splitConjuncts, parseSlotII, parseSlotIV, parseCase
  , isVowelChar, isConsonantCluster
  , vnTable, cnMoodTable, cnCaseScopeTable
  )
import qualified Ithkuil.Allomorph as Ca

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

require :: Maybe a -> Text -> ParseResult a
require (Just a) _ = Success a
require Nothing msg = Failure msg

--------------------------------------------------------------------------------
-- Full Formative Parsing
--------------------------------------------------------------------------------

-- | Parse a complete formative with all 10 slots
parseFormative :: Text -> ParseResult Formative
parseFormative word = do
  let conjuncts = splitConjuncts word
      stress = detectStress word

  when (length conjuncts < 3) $
    Failure "Word too short for formative"

  parseFormativeConjuncts conjuncts stress

-- | Parse from conjunct list
parseFormativeConjuncts :: [Text] -> Stress -> ParseResult Formative
parseFormativeConjuncts conjs stress = do
  -- Check for concatenation marker (Slot I)
  let (slotI, rest1) = parseSlotI conjs

  -- Determine if vowel-initial or consonant-initial
  case rest1 of
    [] -> Failure "Empty formative after concatenation check"
    (first:_)
      | isConsonantCluster first -> parseConsonantInitial slotI rest1 stress
      | otherwise -> parseVowelInitial slotI rest1 stress

-- | Parse vowel-initial formative (explicit Vv)
parseVowelInitial :: Maybe ConcatenationStatus -> [Text] -> Stress -> ParseResult Formative
parseVowelInitial slotI parts stress = case parts of
  (vv:cr:vr:rest) -> do
    slotII <- require (parseSlotII vv) $ "Invalid Vv: " <> vv
    slotIV <- require (parseSlotIV vr) $ "Invalid Vr: " <> vr
    parseAfterVr slotI slotII (Root cr) slotIV rest stress
  (vv:cr:[]) -> do
    slotII <- require (parseSlotII vv) $ "Invalid Vv: " <> vv
    -- Minimal formative: just Vv-Cr (default everything else)
    Success $ Formative slotI slotII (Root cr) defaultSlotIV [] defaultSlotVI [] Nothing (Left (Transrelative THM)) stress
  _ -> Failure "Too few parts for vowel-initial formative"

-- | Parse consonant-initial formative (elided Vv = S1/PRC)
parseConsonantInitial :: Maybe ConcatenationStatus -> [Text] -> Stress -> ParseResult Formative
parseConsonantInitial slotI parts stress = case parts of
  (cr:vr:rest) -> do
    slotIV <- require (parseSlotIV vr) $ "Invalid Vr: " <> vr
    parseAfterVr slotI defaultSlotII (Root cr) slotIV rest stress
  (cr:[]) ->
    -- Just a root, everything default
    Success $ Formative slotI defaultSlotII (Root cr) defaultSlotIV [] defaultSlotVI [] Nothing (Left (Transrelative THM)) stress
  _ -> Failure "Too few parts for consonant-initial formative"

-- | Parse remaining slots after Vr (Slots V through IX)
parseAfterVr :: Maybe ConcatenationStatus -> SlotII -> Root -> SlotIV -> [Text] -> Stress -> ParseResult Formative
parseAfterVr slotI slotII root slotIV remaining stress = do
  -- Separate the remaining conjuncts into groups
  -- Pattern: [CsVx...] Ca [VxCs...] [VnCn] Vc
  -- The Ca is always a consonant cluster, Vc is always a vowel
  let (slotV, slotVI, slotVII, slotVIII, slotIX) = parseRemainingSlots remaining stress
  Success $ Formative slotI slotII root slotIV slotV slotVI slotVII slotVIII slotIX stress

-- | Parse remaining slots after Vr using right-to-left peeling.
--
-- Structure: [CsVx...] Ca [VxCs...] [Vn Cn] [Vc] [Slot X]
--
-- Algorithm:
--   1. Strip trailing consonant (Slot X)
--   2. Strip trailing vowel (Vc / Slot IX)
--   3. Strip trailing Vn+Cn pair if the C is a Cn consonant (Slot VIII)
--   4. Find Ca in what remains using parseCaSlot (first matching consonant)
--   5. Before Ca = Slot V affixes (CsVx pairs)
--   6. After Ca = Slot VII affixes (VxCs pairs)
parseRemainingSlots :: [Text] -> Stress
  -> ([Affix], SlotVI, [Affix], Maybe (Valence, MoodOrScope), Either Case FormatOrIV)
parseRemainingSlots parts0 stress =
  let -- 1. Strip trailing consonant (Slot X)
      p1 = stripTrailingC parts0
      -- 2. Strip trailing vowel (Vc / Slot IX)
      (vc, p2) = stripTrailingV p1
      -- 3. Strip trailing Vn+Cn (Slot VIII) if Cn is recognized
      (vnCn, p3) = stripVnCn p2
      -- 4-6. Split remainder into Slot V, Ca, Slot VII
      (slotVParts, caText, slotVIIParts) = splitAtCa p3
      slotV  = parseCsVxAffixes slotVParts
      slotVI = parseCaOrDefault caText
      slotVII = parseAffixes slotVIIParts
      slotVIII = vnCn
      slotIX = case vc of
        Nothing -> Left (Transrelative THM)
        Just v  -> parseSlotIX v stress
  in (slotV, slotVI, slotVII, slotVIII, slotIX)

-- | Strip trailing consonant (Slot X)
stripTrailingC :: [Text] -> [Text]
stripTrailingC parts = case reverse parts of
  (c:rest) | isC c -> reverse rest
  _ -> parts

-- | Strip trailing vowel (Vc / Slot IX)
stripTrailingV :: [Text] -> (Maybe Text, [Text])
stripTrailingV parts = case reverse parts of
  (v:rest) | isV v -> (Just v, reverse rest)
  _ -> (Nothing, parts)

-- | Strip trailing Vn+Cn (Slot VIII) if the consonant is a Cn form
stripVnCn :: [Text] -> (Maybe (Valence, MoodOrScope), [Text])
stripVnCn parts = case reverse parts of
  (cn:vn:rest) | isC cn && isCn cn && isV vn ->
    (Just (parseVnCn vn cn), reverse rest)
  _ -> (Nothing, parts)

-- | Find Ca by scanning for the first consonant that parses as valid Ca.
-- Returns (before Ca, Ca text, after Ca). If no Ca found, returns default.
splitAtCa :: [Text] -> ([Text], Text, [Text])
splitAtCa = go []
  where
    go acc [] = (reverse acc, "", [])
    go acc (c:rest)
      | isC c, Just _ <- Ca.parseCaSlot c = (reverse acc, c, rest)
      | otherwise = go (c : acc) rest

--------------------------------------------------------------------------------
-- Slot I: Concatenation
--------------------------------------------------------------------------------

parseSlotI :: [Text] -> (Maybe ConcatenationStatus, [Text])
parseSlotI ("h":rest)  = (Just Type1, rest)
parseSlotI ("hw":rest) = (Just Type2, rest)
parseSlotI ("w":rest)  = (Just Type1, rest)
parseSlotI rest        = (Nothing, rest)

--------------------------------------------------------------------------------
-- Ca Parsing (Slot VI)
--------------------------------------------------------------------------------

parseCaOrDefault :: Text -> SlotVI
parseCaOrDefault ca = case Ca.parseCaSlot ca of
  Just s -> s
  Nothing -> defaultSlotVI

--------------------------------------------------------------------------------
-- Affix Parsing (Slots V and VII)
--------------------------------------------------------------------------------

-- | Parse affixes from alternating vowel-consonant conjuncts
-- Affixes are VxCs (vowel determines degree, consonant determines type)
parseAffixes :: [Text] -> [Affix]
parseAffixes [] = []
parseAffixes [_] = []  -- Need at least V+C pair
parseAffixes (v:c:rest)
  | not (T.null v) && isVowelChar (T.head v) && isConsonantCluster c =
    Affix v c Type1Affix : parseAffixes rest
  | otherwise = []

--------------------------------------------------------------------------------
-- VnCn Parsing (Slot VIII)
--------------------------------------------------------------------------------

parseVnCn :: Text -> Text -> (Valence, MoodOrScope)
parseVnCn vn cn =
  let val = case listToMaybe [v | (v, f) <- vnTable, f == vn] of
              Just v -> v
              Nothing -> MNO
      ms = case listToMaybe [MoodVal m | (m, f) <- cnMoodTable, f == cn] of
              Just m -> m
              Nothing -> case listToMaybe [CaseScope cs | (cs, f) <- cnCaseScopeTable, f == cn] of
                Just cs -> cs
                Nothing -> MoodVal FAC
  in (val, ms)

--------------------------------------------------------------------------------
-- Slot IX: Case (Vc) or Illocution+Validation (Vk)
--------------------------------------------------------------------------------

parseSlotIX :: Text -> Stress -> Either Case FormatOrIV
parseSlotIX "" _ = Left (Transrelative THM)
parseSlotIX vc Penultimate = case parseCase vc of
  Just c -> Left c
  Nothing -> Left (Transrelative THM)
parseSlotIX vc Ultimate = Right (parseVk vc)
parseSlotIX vc Antepenultimate = case parseCase vc of
  Just c -> Left c
  Nothing -> Left (Transrelative THM)

-- | Parse Vk (Illocution + Validation) from ultimate-stress formatives
parseVk :: Text -> FormatOrIV
parseVk _ = Format  -- TODO: full Vk parsing

--------------------------------------------------------------------------------
-- Stress Detection
--------------------------------------------------------------------------------

-- | Detect stress pattern from word
detectStress :: Text -> Stress
detectStress word =
  let syllables = countSyllables word
      acutePos = findAcuteStress word
  in case acutePos of
    Nothing -> Penultimate  -- Default
    Just pos
      | pos == syllables -> Ultimate
      | pos <= syllables - 2 -> Antepenultimate
      | otherwise -> Penultimate

-- | Count syllables (vowel nuclei)
countSyllables :: Text -> Int
countSyllables = length . filter isVowelPart . splitConjuncts
  where
    isVowelPart t = not (T.null t) && isVowelChar (T.head t)

-- | Find position of acute accent (1-indexed from start)
findAcuteStress :: Text -> Maybe Int
findAcuteStress word =
  let acuteVowels = "áéíóú" :: String
      parts = splitConjuncts word
      vowelParts = zip [1..] $ filter isVowelPart parts
      isVowelPart t = not (T.null t) && isVowelChar (T.head t)
      hasAcute t = T.any (`elem` acuteVowels) t
  in case filter (hasAcute . snd) vowelParts of
    ((pos, _):_) -> Just pos
    [] -> Nothing

--------------------------------------------------------------------------------
-- Slot V Affixes (CsVx pairs: consonant then vowel)
--------------------------------------------------------------------------------

-- | Parse Slot V affixes from alternating consonant-vowel conjuncts
parseCsVxAffixes :: [Text] -> [Affix]
parseCsVxAffixes [] = []
parseCsVxAffixes [_] = []
parseCsVxAffixes (c:v:rest)
  | isC c && isV v = Affix v c Type1Affix : parseCsVxAffixes rest
  | otherwise = []

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Is this conjunct a consonant cluster?
isC :: Text -> Bool
isC = isConsonantCluster

-- | Is this conjunct a vowel group?
isV :: Text -> Bool
isV t = not (T.null t) && isVowelChar (T.head t)

-- | Is this consonant a Cn form (Slot VIII mood/case-scope marker)?
-- Cn consonants: h, hl, hr, hw, hm, hn, hň, w, y
isCn :: Text -> Bool
isCn c = c `elem` cnForms
  where
    cnForms :: [Text]
    cnForms = map snd cnMoodTable ++ map snd cnCaseScopeTable

when :: Bool -> ParseResult () -> ParseResult ()
when True m = m
when False _ = Success ()
