{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Full Formative Parser
-- Complete 10-slot parsing with stress analysis
module Ithkuil.FullParse where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import Ithkuil.Grammar
import Ithkuil.Parse (splitConjuncts, parseSlotII, parseSlotIV,
                      parseCa, ParsedCa(..), isVowelChar)
import qualified Ithkuil.Parse as P
import Ithkuil.Phonology (vowelFormLookup)

--------------------------------------------------------------------------------
-- Parser Types
--------------------------------------------------------------------------------

data ParseResult a
  = Success a
  | Failure Text  -- Error message
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
-- Full Formative Parsing
--------------------------------------------------------------------------------

-- | Parse a complete formative with all 10 slots
parseFormative :: Text -> ParseResult Formative
parseFormative word = do
  let conjuncts = splitConjuncts word
      stress = detectStress word

  -- Minimum: Vv-Cr-Vr-Ca (4 parts)
  when (length conjuncts < 4) $
    Failure "Word too short for formative"

  -- Parse based on structure
  parseFormativeConjuncts conjuncts stress

-- | Parse from conjunct list
parseFormativeConjuncts :: [Text] -> Stress -> ParseResult Formative
parseFormativeConjuncts conjs stress = do
  -- Check for concatenation marker (Slot I)
  let (slotI, rest1) = parseSlotI conjs

  -- Parse Vv (Slot II)
  (slotII, rest2) <- case rest1 of
    (v:rest) -> case parseSlotII v of
      Just s2 -> Success (s2, rest)
      Nothing -> Failure $ "Invalid Vv: " <> v
    [] -> Failure "Missing Vv slot"

  -- Parse Cr (Slot III - Root)
  (root, rest3) <- case rest2 of
    (c:rest) -> Success (Root c, rest)
    [] -> Failure "Missing Cr (root)"

  -- Parse Vr (Slot IV)
  (slotIV, rest4) <- case rest3 of
    (v:rest) -> case parseSlotIV v of
      Just s4 -> Success (s4, rest)
      Nothing -> Failure $ "Invalid Vr: " <> v
    [] -> Failure "Missing Vr slot"

  -- Parse remaining slots (V, VI, VII, VIII, IX)
  (slotV, slotVI, slotVII, slotVIII, slotIX) <- parseRemainingSlots rest4 stress

  Success $ Formative
    { fSlotI = slotI
    , fSlotII = slotII
    , fSlotIII = root
    , fSlotIV = slotIV
    , fSlotV = slotV
    , fSlotVI = slotVI
    , fSlotVII = slotVII
    , fSlotVIII = slotVIII
    , fSlotIX = slotIX
    , fStress = stress
    }

-- | Parse Slot I (Concatenation status)
parseSlotI :: [Text] -> (Maybe ConcatenationStatus, [Text])
parseSlotI ("h":rest)  = (Just Type1, rest)
parseSlotI ("hw":rest) = (Just Type2, rest)
parseSlotI rest        = (Nothing, rest)

-- | Parse remaining slots after Vr
parseRemainingSlots :: [Text] -> Stress -> ParseResult ([Affix], SlotVI, [Affix], Maybe SlotVIII, Either Case FormatOrIV)
parseRemainingSlots parts stress = do
  case parts of
    -- Minimal (just Ca)
    [ca] -> do
      slotVI <- parseFullCa ca
      Success ([], slotVI, [], Nothing, Left (Transrelative THM))

    -- Ca + Vc (no affixes)
    [ca, vc] -> do
      slotVI <- parseFullCa ca
      slotIX <- parseSlotIX vc stress
      Success ([], slotVI, [], Nothing, slotIX)

    -- 3+ parts: parse Ca, then remaining V-C pairs as Slot VII affixes.
    -- The key insight: after Ca, alternating V-C chunks are VxCs affix pairs.
    -- If the sequence ends in a vowel (no trailing C), the final V is Vc.
    -- If it ends in a consonant, Vc is elided (THM default).
    -- Special case: VnCn (Slot VIII) can consume a V-C pair if the C is a valid Cn.
    _
      -- First: identify Slot V (if parts start with vowel before Ca)
      | isVowelStart (head parts) -> do
          let (slotVParts, caParts) = splitSlotV parts
          slotV <- parseAffixes (head slotVParts)
          parseCaAndAfter (tail caParts) slotV stress
      -- Otherwise: first part is Ca
      | otherwise -> do
          parseCaAndAfter parts [] stress

    _ -> Failure $ "Unexpected slot structure: " <> T.pack (show $ length parts) <> " parts"

-- | Split off Slot V vowel parts from the beginning of the part list.
-- Slot V is present when the sequence starts with a vowel (before Ca consonant).
splitSlotV :: [Text] -> ([Text], [Text])
splitSlotV (v:rest)
  | isVowelStart v = ([v], rest)
splitSlotV xs = ([], xs)

-- | Parse Ca and everything after it: Slot VII affixes, optional VnCn, and Vc.
-- The parts list starts with Ca, followed by alternating V-C chunks.
-- Rule: V-C pairs after Ca are Slot VII affixes.
-- If the sequence ends in a final vowel (not followed by C), it's Vc.
-- If it ends in a consonant, Vc is elided (THM).
-- A V-C pair where C is a valid Cn form may be VnCn (Slot VIII) instead.
parseCaAndAfter :: [Text] -> [Affix] -> Stress -> ParseResult ([Affix], SlotVI, [Affix], Maybe SlotVIII, Either Case FormatOrIV)
parseCaAndAfter [] _slotV _stress =
  Failure "Expected Ca"
parseCaAndAfter [ca] slotV _stress = do
  slotVI <- parseFullCa ca
  Success (slotV, slotVI, [], Nothing, Left (Transrelative THM))
parseCaAndAfter (ca:rest) slotV stress = do
  slotVI <- parseFullCa ca
  -- Pair up remaining V-C chunks as affixes, handle final Vc
  let (affixPairs, trailing) = pairVCChunks rest
  -- Check: does trailing end with a vowel? If so, it's Vc.
  -- Does it end with a consonant? Then Vc is elided (THM).
  case trailing of
    -- No trailing: all paired as affixes, Vc elided
    [] -> do
      slotVII <- parseAffixPairs affixPairs
      Success (slotV, slotVI, slotVII, Nothing, Left (Transrelative THM))
    -- Trailing vowel: it's the Vc
    [vc] | isVowelStart vc -> do
      slotVII <- parseAffixPairs affixPairs
      slotIX <- parseSlotIX vc stress
      Success (slotV, slotVI, slotVII, Nothing, slotIX)
    -- Trailing V-C: could be VnCn (if C is valid Cn) or last affix + elided Vc
    [vn, cn] | isValidCn cn -> do
      slotVII <- parseAffixPairs affixPairs
      slotVIII <- parseVnCnFromParts vn cn stress
      Success (slotV, slotVI, slotVII, slotVIII, Left (Transrelative THM))
    -- Trailing V-C where C is not a valid Cn: treat as last affix, Vc elided
    [vx, cs] -> do
      slotVII <- parseAffixPairs (affixPairs ++ [(vx, cs)])
      Success (slotV, slotVI, slotVII, Nothing, Left (Transrelative THM))
    -- Trailing V-C-V: last affix + Vc
    [vx, cs, vc] -> do
      slotVII <- parseAffixPairs (affixPairs ++ [(vx, cs)])
      slotIX <- parseSlotIX vc stress
      Success (slotV, slotVI, slotVII, Nothing, slotIX)
    -- Trailing V-C-V-C: VnCn + Vc, or two more affixes
    [v1, c1, v2, c2] | isValidCn c1 -> do
      slotVII <- parseAffixPairs affixPairs
      slotVIII <- parseVnCnFromParts v1 c1 stress
      case slotVIII of
        Just s8 -> do
          -- c1 was VnCn, so v2+c2 is another affix? No, after VnCn comes Vc.
          slotIX <- parseSlotIX v2 stress
          Success (slotV, slotVI, slotVII, Just s8, slotIX)
        Nothing -> do
          -- Not valid VnCn, treat both as affixes
          slotVII2 <- parseAffixPairs (affixPairs ++ [(v1, c1), (v2, c2)])
          Success (slotV, slotVI, slotVII2, Nothing, Left (Transrelative THM))
    -- Fallback: pair as many affixes as possible
    other -> do
      let (morePairs, finalTrail) = pairVCChunks other
          allPairs = affixPairs ++ morePairs
      slotVII <- parseAffixPairs allPairs
      case finalTrail of
        [vc] | isVowelStart vc -> do
          slotIX <- parseSlotIX vc stress
          Success (slotV, slotVI, slotVII, Nothing, slotIX)
        _ ->
          Success (slotV, slotVI, slotVII, Nothing, Left (Transrelative THM))

-- | Pair alternating V-C chunks into (Vx, Cs) affix pairs.
-- Returns paired chunks and any unpaired trailing chunks.
pairVCChunks :: [Text] -> ([(Text, Text)], [Text])
pairVCChunks (v:c:rest)
  | isVowelStart v && isConsonant c = let (pairs, trail) = pairVCChunks rest
                                       in ((v, c) : pairs, trail)
pairVCChunks rest = ([], rest)

-- | Parse a list of (Vx, Cs) pairs into Affix values
parseAffixPairs :: [(Text, Text)] -> ParseResult [Affix]
parseAffixPairs pairs = Success $ map (\(vx, cs) -> parseOneAffix vx cs) pairs

-- | Parse a single Vx+Cs affix pair
parseOneAffix :: Text -> Text -> Affix
parseOneAffix vx cs =
  let (atype, _deg) = classifyAffixVowel vx
  in Affix vx cs atype

-- | Check if a consonant is a valid Cn form (Slot VIII mood/case-scope)
-- Valid Cn: h, hl, hr, hm, hn, hň (Pattern 1) or w, y, hw, hrw, hmw, hnw, hňw (Pattern 2)
isValidCn :: Text -> Bool
isValidCn t = t `elem` ["h", "hl", "hr", "hm", "hn", "hň",
                         "w", "y", "hw", "hrw", "hmw", "hnw", "hňw"]

--------------------------------------------------------------------------------
-- Slot Parsers
--------------------------------------------------------------------------------

-- | Parse Ca (Slot VI) - returns full SlotVI tuple
parseFullCa :: Text -> ParseResult SlotVI
parseFullCa ca = case parseCa ca of
  Just pc -> Success (pcConfig pc, pcAffiliation pc, pcPerspective pc, pcExtension pc, pcEssence pc)
  Nothing -> Success defaultSlotVI  -- Fall back to default if unrecognized

-- | Parse affixes from a vowel+consonant or consonant+vowel sequence
-- VxCs format: vowel (degree) + consonant (affix type)
-- CsVx format: consonant (affix type) + vowel (degree) [reversed in Slot V]
parseAffixes :: Text -> ParseResult [Affix]
parseAffixes t
  | T.null t = Success []
  | otherwise =
    -- Split into individual affixes (each is a V+C or C+V pair)
    let conjs = splitConjuncts t
    in Success $ pairConjunctAffixes conjs

-- | Parse conjunct pairs into affixes (low-level, used by parseAffixes)
pairConjunctAffixes :: [Text] -> [Affix]
pairConjunctAffixes [] = []
pairConjunctAffixes [_] = []  -- Lone conjunct, can't form a pair
pairConjunctAffixes (v:c:rest)
  | isVowelStart v && isConsonant c =
    let (affType, _) = classifyAffixVowel v
    in Affix v c affType : pairConjunctAffixes rest
  | isConsonant v && isVowelStart c =
    -- CsVx (reversed) format used in Slot V
    let (affType, _) = classifyAffixVowel c
    in Affix c v affType : pairConjunctAffixes rest
  | otherwise = pairConjunctAffixes (c:rest)  -- Skip unmatched

-- | Classify affix vowel to determine type and degree
-- Returns (AffixType, degree 0-9)
classifyAffixVowel :: Text -> (AffixType, Int)
classifyAffixVowel v =
  case lookupDegree type1Degrees v of
    Just d -> (Type1Affix, d)
    Nothing -> case lookupDegree type2Degrees v of
      Just d -> (Type2Affix, d)
      Nothing -> case lookupDegree type3Degrees v of
        Just d -> (Type3Affix, d)
        Nothing -> (Type1Affix, 0)  -- Unknown, default

lookupDegree :: [(Text, Int)] -> Text -> Maybe Int
lookupDegree table v = lookup v table

-- | Type 1 degree vowels (Series 1)
type1Degrees :: [(Text, Int)]
type1Degrees =
  [ ("a", 1), ("ä", 2), ("e", 3), ("i", 4), ("ëi", 5)
  , ("ö", 6), ("o", 7), ("ü", 8), ("u", 9), ("ae", 0)
  ]

-- | Type 2 degree vowels (Series 2)
type2Degrees :: [(Text, Int)]
type2Degrees =
  [ ("ai", 1), ("au", 2), ("ei", 3), ("eu", 4), ("ëu", 5)
  , ("ou", 6), ("oi", 7), ("iu", 8), ("ui", 9), ("ea", 0)
  ]

-- | Type 3 degree vowels (Series 3, with alternates)
type3Degrees :: [(Text, Int)]
type3Degrees =
  [ ("ia", 1), ("uä", 1), ("ie", 2), ("uë", 2)
  , ("io", 3), ("üä", 3), ("iö", 4), ("üë", 4)
  , ("eë", 5)
  , ("uö", 6), ("öë", 6), ("uo", 7), ("öä", 7)
  , ("ue", 8), ("ië", 8), ("ua", 9), ("iä", 9)
  , ("üo", 0)
  ]

-- | Parse VnCn from two conjunct parts (Vn vowel + Cn consonant)
-- Pattern 1 (Cn = h/hl/hr/hm/hn/hň): Vn = Valence or Phase
-- Pattern 2 (Cn = w/y/hw/hrw/hmw/hnw/hňw): Vn = Aspect
parseVnCnFromParts :: Text -> Text -> Stress -> ParseResult (Maybe SlotVIII)
parseVnCnFromParts vn cn _ =
  let moodOrScope = case parseCnMood cn of
        Just mood -> Just (MoodVal mood)
        Nothing -> case parseCnCaseScope cn of
          Just cs -> Just (CaseScope cs)
          Nothing -> Nothing
      isPattern2 = cn `elem` ["w", "y", "hw", "hrw", "hmw", "hnw", "hňw"]
  in case moodOrScope of
    Nothing -> Success Nothing
    Just ms
      | isPattern2 ->
        -- Pattern 2: Vn is Aspect
        case lookup vn aspectVowels of
          Just asp -> Success $ Just (VnCnAspect asp ms)
          Nothing -> Success Nothing
      | otherwise ->
        -- Pattern 1: Vn is Valence (Series 1), Phase (Series 2), Effect (Series 3), or Level (Series 4)
        case parseVnValence vn of
          Just val -> Success $ Just (VnCnValence val ms)
          Nothing -> case lookup vn phaseVowels of
            Just ph -> Success $ Just (VnCnPhase ph ms)
            Nothing -> case lookup vn effectVowels of
              Just eff -> Success $ Just (VnCnEffect eff ms)
              Nothing -> case lookup vn levelVowels of
                Just lvl -> Success $ Just (VnCnLevel lvl False ms)
                Nothing -> Success Nothing

--------------------------------------------------------------------------------
-- Vn: Valence / Phase / Effect parsing
--------------------------------------------------------------------------------

-- | Parse Vn vowel as Valence (Pattern 1, Series 1)
parseVnValence :: Text -> Maybe Valence
parseVnValence v = lookup v valenceVowels

valenceVowels :: [(Text, Valence)]
valenceVowels =
  [ ("a",  MNO), ("ä",  PRL), ("e",  CRO), ("i",  RCP)
  , ("ëi", CPL), ("ö",  DUP), ("o",  DEM), ("ü",  CNG), ("u",  PTI)
  ]

-- | Phase vowels (Pattern 1, Series 2)
phaseVowels :: [(Text, Phase)]
phaseVowels =
  [ ("ai", PCT), ("au", ITR), ("ei", REP), ("eu", ITM)
  , ("ëu", RCT), ("ou", FRE), ("oi", FRG), ("iu", VAC), ("ui", FLC)
  ]

-- | Effect vowels (Pattern 1, Series 3)
effectVowels :: [(Text, Effect)]
effectVowels =
  [ ("ia", BEN1), ("ie", BEN2), ("io", BEN3), ("iö", BSLF)
  , ("eë", UNK),  ("uö", DSLF), ("uo", DET3), ("ue", DET2), ("ua", DET1)
  -- Alternative series 3 forms
  , ("uä", BEN1), ("uë", BEN2), ("üä", BEN3), ("üë", BSLF)
  , ("öë", DSLF), ("öä", DET3), ("ië", DET2), ("iä", DET1)
  ]

-- | Level vowels (Pattern 1, Series 4)
levelVowels :: [(Text, Level)]
levelVowels =
  [ ("ao", MIN), ("aö", SBE), ("eo", IFR), ("eö", DFT), ("oë", EQU)
  , ("öe", SUR), ("oe", SPL), ("öa", SPQ), ("oa", MAX)
  ]

--------------------------------------------------------------------------------
-- Cn: Mood / Case-Scope parsing
--------------------------------------------------------------------------------

-- | Pattern 1 Cn consonants for Mood
parseCnMood :: Text -> Maybe Mood
parseCnMood "h"  = Just FAC
parseCnMood "hl" = Just SUB
parseCnMood "hr" = Just ASM
parseCnMood "hm" = Just SPC
parseCnMood "hn" = Just COU
parseCnMood "hň" = Just HYP
parseCnMood _    = Nothing

-- | Pattern 2 Cn consonants for Mood (with aspect Vn)
parseCnMoodP2 :: Text -> Maybe Mood
parseCnMoodP2 "w"   = Just FAC
parseCnMoodP2 "y"   = Just FAC
parseCnMoodP2 "hw"  = Just SUB
parseCnMoodP2 "hrw" = Just ASM
parseCnMoodP2 "hmw" = Just SPC
parseCnMoodP2 "hnw" = Just COU
parseCnMoodP2 "hňw" = Just HYP
parseCnMoodP2 _     = Nothing

-- | Parse Cn as Case-Scope
parseCnCaseScope :: Text -> Maybe CaseScope
parseCnCaseScope "h"   = Just CCN  -- Same as FAC
parseCnCaseScope "w"   = Just CCN
parseCnCaseScope "y"   = Just CCN
parseCnCaseScope "hl"  = Just CCA
parseCnCaseScope "hw"  = Just CCA
parseCnCaseScope "hr"  = Just CCS
parseCnCaseScope "hrw" = Just CCS
parseCnCaseScope "hm"  = Just CCQ
parseCnCaseScope "hmw" = Just CCQ
parseCnCaseScope "hn"  = Just CCP
parseCnCaseScope "hnw" = Just CCP
parseCnCaseScope "hň"  = Just CCV
parseCnCaseScope "hňw" = Just CCV
parseCnCaseScope _     = Nothing

-- | All valid Cn consonants (for Pattern 1 and 2)
cnConsonants :: [Text]
cnConsonants = ["h", "hl", "hr", "hm", "hn", "hň", "w", "y", "hw", "hrw", "hmw", "hnw", "hňw"]

--------------------------------------------------------------------------------
-- Aspect parsing (Pattern 2 Vn)
--------------------------------------------------------------------------------

-- | All 36 aspects organized in 4 columns of 9
-- Column 1: series 1 vowels, Column 2: series 2, Column 3: series 3, Column 4: series 4
aspectVowels :: [(Text, Aspect)]
aspectVowels =
  -- Column 1 (retrospective through anticipatory)
  [ ("a",  RTR), ("ä",  PRS), ("e",  HAB), ("i",  PRG)
  , ("ëi", IMM), ("ö",  PCS), ("o",  REG), ("ü",  SMM), ("u",  ATP)
  -- Column 2 (resumptive through interruptive)
  , ("ai", RSM), ("au", CSS), ("ei", PAU), ("eu", RGR)
  , ("ëu", PCL), ("ou", CNT), ("oi", ICS), ("iu", EXP), ("ui", IRP)
  -- Column 3 (preemptive through preparatory)
  , ("ia", PMP),  ("ie", CLM),  ("io", DLT),  ("iö", TMP)
  , ("eë", XPD),  ("uö", LIM),  ("uo", EPD),  ("ue", PTC), ("ua", PPR)
  -- Alternative series 3 forms
  , ("uä", PMP),  ("uë", CLM),  ("üä", DLT),  ("üë", TMP)
  , ("öë", LIM),  ("öä", EPD),  ("ië", PTC),  ("iä", PPR)
  -- Column 4 (disclusive through sequential)
  , ("ao", DCL), ("aö", CCL), ("eo", CUL), ("eö", IMD)
  , ("oë", TRD), ("öe", TNS), ("oe", ITC), ("öa", MTV), ("oa", SQN)
  ]

--------------------------------------------------------------------------------
-- Vk: Illocution + Validation parsing (Slot IX when stress is ultimate)
--------------------------------------------------------------------------------

-- | Parse Slot IX based on stress
parseSlotIX :: Text -> Stress -> ParseResult (Either Case FormatOrIV)
parseSlotIX vc Monosyllabic = Left <$> parseFullCase vc
parseSlotIX vc Penultimate = Left <$> parseFullCase vc
parseSlotIX vk Ultimate = Right <$> parseVk vk
parseSlotIX vc Antepenultimate = Left <$> parseFullCase vc

-- | Parse Vk vowel as Illocution + Validation
-- Series 1 = ASR + Validation (form 1-9)
-- Series 2 = Other illocutions by form (no Validation)
parseVk :: Text -> ParseResult FormatOrIV
parseVk vk =
  case seriesAndForm vk of
    Just (1, form) ->
      let validation = case form of
            1 -> Just OBS; 2 -> Just REC; 3 -> Just PUP; 4 -> Just RPR
            5 -> Just USP; 6 -> Just IMA; 7 -> Just CVN; 8 -> Just ITU
            9 -> Just INF; _ -> Nothing
      in case validation of
           Just v -> Success $ IllocVal ASR v
           Nothing -> Failure $ "Invalid Vk form: " <> vk
    Just (2, form) ->
      let illocution = case form of
            1 -> Just DIR; 2 -> Just DEC; 3 -> Just IRG; 4 -> Just VER
            6 -> Just ADM; 7 -> Just POT; 8 -> Just HOR; 9 -> Just CNJ
            _ -> Nothing
      in case illocution of
           Just ill -> Success $ IllocVal ill OBS  -- Default validation
           Nothing -> Failure $ "Invalid Vk form: " <> vk
    _ -> Failure $ "Invalid Vk: " <> vk

-- | Determine series (1-4) and form (1-9) from a vowel using the vowel form table
seriesAndForm :: Text -> Maybe (Int, Int)
seriesAndForm = vowelFormLookup

--------------------------------------------------------------------------------
-- Case parsing (full 68 cases)
--------------------------------------------------------------------------------

-- | Parse case from Vc vowel - delegates to Parse module's casePatterns
parseFullCase :: Text -> ParseResult Case
parseFullCase vc = case P.parseCase vc of
  Just c -> Success c
  Nothing -> Failure $ "Unknown case: " <> vc

--------------------------------------------------------------------------------
-- Stress Detection
--------------------------------------------------------------------------------

-- | Detect stress pattern from word
detectStress :: Text -> Stress
detectStress word =
  let syllables = countSyllables word
      acutePos = findAcuteStress word
  in case acutePos of
    Nothing | syllables <= 1 -> Monosyllabic
            | otherwise -> Penultimate
    Just pos
      | pos == syllables -> Ultimate
      | pos == syllables - 2 -> Antepenultimate
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
      chars = T.unpack word
      vowelPositions = [i | (i, c) <- zip [1 :: Int ..] chars, c `elem` ("aäeëiöoüu" ++ acuteVowels)]
      acutePositions = [i | (i, c) <- zip [1 :: Int ..] chars, c `elem` acuteVowels]
  in case acutePositions of
    [] -> Nothing
    (p:_) -> Just (length vowelPositions - length (filter (>p) vowelPositions))

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

when :: Bool -> ParseResult () -> ParseResult ()
when True m = m
when False _ = Success ()

isConsonant :: Text -> Bool
isConsonant t = case T.uncons t of
  Nothing -> False
  Just (c, _) -> not (isVowelChar c)

isVowelStart :: Text -> Bool
isVowelStart t = case T.uncons t of
  Nothing -> False
  Just (c, _) -> isVowelChar c
