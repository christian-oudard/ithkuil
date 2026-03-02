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

-- | Parse remaining slots after Vr
-- Returns (SlotV affixes, SlotVI Ca, SlotVII affixes, SlotVIII VnCn, SlotIX Vc/Vk)
parseRemainingSlots :: [Text] -> Stress
  -> ([Affix], SlotVI, [Affix], Maybe (Valence, MoodOrScope), Either Case FormatOrIV)
parseRemainingSlots parts stress = case classifyParts parts of
  -- Just Ca (minimal)
  CaParts ca ->
    ([], parseCaOrDefault ca, [], Nothing, Left (Transrelative THM))

  -- Ca + Vc
  CaVcParts ca vc ->
    ([], parseCaOrDefault ca, [], Nothing, parseSlotIX vc stress)

  -- CsVx + Ca + Vc (with slot V affixes)
  CsVxCaVcParts csvx ca vc ->
    (parseAffixes csvx, parseCaOrDefault ca, [], Nothing, parseSlotIX vc stress)

  -- Ca + VxCs + Vc (with slot VII affixes)
  CaVxCsVcParts ca vxcs vc ->
    ([], parseCaOrDefault ca, parseAffixes vxcs, Nothing, parseSlotIX vc stress)

  -- Ca + VxCs + VnCn + Vc (with slot VII affixes and mood/valence)
  CaVxCsVnCnVcParts ca vxcs vn cn vc ->
    ([], parseCaOrDefault ca, parseAffixes vxcs, Just (parseVnCn vn cn), parseSlotIX vc stress)

  -- Fallback: treat first consonant as Ca, last vowel as Vc
  UnknownParts ->
    ([], defaultSlotVI, [], Nothing, Left (Transrelative THM))

-- | Classify the remaining parts pattern
data PartsPattern
  = CaParts Text
  | CaVcParts Text Text
  | CsVxCaVcParts [Text] Text Text
  | CaVxCsVcParts Text [Text] Text
  | CaVxCsVnCnVcParts Text [Text] Text Text Text
  | UnknownParts

classifyParts :: [Text] -> PartsPattern
classifyParts parts =
  let isC = isConsonantCluster
      isV t = not (T.null t) && isVowelChar (T.head t)
  in case parts of
    -- Just Ca
    [ca] | isC ca -> CaParts ca

    -- Ca + Vc
    [ca, vc] | isC ca && isV vc -> CaVcParts ca vc

    -- Three parts: could be CsVx+Ca+Vc or Ca+VxCs+Vc
    -- If C-V-C-V: first pair is CsVx affix, then Ca, then Vc
    -- If C-C-V: first C is Ca, second C is... ambiguous
    -- Simplification: treat as Ca-VxCs-Vc if second is consonant
    [a, b, vc] | isC a && isV b && isC vc ->
      -- V is affix vowel, so this is affix material... but Vc should be vowel
      UnknownParts
    [ca, vxcs, vc] | isC ca && isC vxcs && isV vc ->
      -- Two consonant clusters then vowel: Ca + trailing C + Vc?
      -- Treat first as Ca
      CaVcParts ca vc
    [csvx, ca, vc] | isV csvx && isC ca && isV vc ->
      -- V-C-V after Vr: the V could be affix vowel before Ca
      -- But we don't have the affix consonant... so treat as Vc-Ca-Vc?
      -- Actually this is the Vr-internal pattern. Skip affix, use Ca+Vc
      CaVcParts ca vc

    -- Four parts
    [csv, cs, ca, vc] | isV csv && isC cs && isC ca && isV vc ->
      -- Affix: VxCs before Ca
      CsVxCaVcParts [csv, cs] ca vc
    [ca, vx, cs, vc] | isC ca && isV vx && isC cs && isV vc ->
      -- Ca, then VxCs affix, then Vc
      CaVxCsVcParts ca [vx, cs] vc

    -- Five parts
    [ca, vx, cs, vn, cn] | isC ca && isV vx && isC cs && isV vn && isC cn ->
      -- Ca + VxCs + Vn + Cn (no final Vc, or Cn subsumes it)
      CaVxCsVnCnVcParts ca [vx, cs] vn cn ""

    -- Six parts
    [csv, cs, ca, vx2, cs2, vc]
      | isV csv && isC cs && isC ca && isV vx2 && isC cs2 && isV vc ->
      CsVxCaVcParts [csv, cs] ca vc  -- simplified: ignore slot VII
    [ca, vx, cs, vn, cn, vc]
      | isC ca && isV vx && isC cs && isV vn && isC cn && isV vc ->
      CaVxCsVnCnVcParts ca [vx, cs] vn cn vc

    _ -> UnknownParts

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
-- Helper
--------------------------------------------------------------------------------

when :: Bool -> ParseResult () -> ParseResult ()
when True m = m
when False _ = Success ()
