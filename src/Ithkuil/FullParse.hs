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
import Ithkuil.Phonology (vowelFormTable)

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
parseRemainingSlots :: [Text] -> Stress -> ParseResult ([Affix], SlotVI, [Affix], Maybe (Valence, MoodOrScope), Either Case FormatOrIV)
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

    -- 3 parts: could be CsVx-Ca-Vc or Ca-VxCs-Vc
    -- Heuristic: if first is consonant cluster and second is consonant cluster, it's CsVx-Ca-Vc
    -- If first is consonant and second starts with vowel, it's Ca-VxCs-Vc
    [p1, p2, p3]
      | isConsonant p1 && isConsonant p2 -> do
          -- CsVx-Ca-Vc (but CsVx should be C+V, so this is ambiguous)
          -- For now treat as Ca-VxCs-Vc if p2 starts with vowel
          slotVI <- parseFullCa p1
          slotVII <- parseAffixes p2
          slotIX <- parseSlotIX p3 stress
          Success ([], slotVI, slotVII, Nothing, slotIX)
      | isConsonant p1 -> do
          slotVI <- parseFullCa p1
          slotIX <- parseSlotIX p3 stress
          Success ([], slotVI, [], Nothing, slotIX)
      | otherwise -> do
          slotV <- parseAffixes p1
          slotVI <- parseFullCa p2
          slotIX <- parseSlotIX p3 stress
          Success (slotV, slotVI, [], Nothing, slotIX)

    -- 4 parts: could be Ca-VxCs-VnCn-Vc or CsVx-Ca-VxCs-Vc
    [p1, p2, p3, p4]
      | isConsonant p1 && isVowelStart p2 -> do
          -- Ca-VxCs-VnCn-Vc
          slotVI <- parseFullCa p1
          slotVII <- parseAffixes p2
          slotVIII <- parseVnCnFromParts p3 p4 stress
          case slotVIII of
            Just s8 -> do
              Success ([], slotVI, slotVII, Just s8, Left (Transrelative THM))
            Nothing -> do
              slotIX <- parseSlotIX p4 stress
              Success ([], slotVI, slotVII, Nothing, slotIX)
      | otherwise -> do
          slotV <- parseAffixes p1
          slotVI <- parseFullCa p2
          slotVII <- parseAffixes p3
          slotIX <- parseSlotIX p4 stress
          Success (slotV, slotVI, slotVII, Nothing, slotIX)

    -- 5+ parts: full form with VnCn
    (p1:p2:p3:p4:p5:_) -> do
      slotV <- parseAffixes p1
      slotVI <- parseFullCa p2
      slotVII <- parseAffixes p3
      slotVIII <- parseVnCnFromParts p4 p5 stress
      let lastPart = last parts
      slotIX <- parseSlotIX lastPart stress
      Success (slotV, slotVI, slotVII, slotVIII, slotIX)

    _ -> Failure $ "Unexpected slot structure: " <> T.pack (show $ length parts) <> " parts"

--------------------------------------------------------------------------------
-- Slot Parsers
--------------------------------------------------------------------------------

-- | Parse Ca (Slot VI) - returns full SlotVI tuple
parseFullCa :: Text -> ParseResult SlotVI
parseFullCa ca = case parseCa ca of
  Just pc -> Success (pcConfig pc, pcAffiliation pc, pcPerspective pc, pcExtension pc, pcEssence pc)
  Nothing -> Success defaultSlotVI  -- Fall back to default if unrecognized

-- | Parse affixes from a vowel+consonant or consonant+vowel sequence
parseAffixes :: Text -> ParseResult [Affix]
parseAffixes t
  | T.null t = Success []
  | otherwise = Success [Affix t "" Type1Affix]  -- Basic: store raw form for now

-- | Parse VnCn from two conjunct parts (Vn vowel + Cn consonant)
parseVnCnFromParts :: Text -> Text -> Stress -> ParseResult (Maybe (Valence, MoodOrScope))
parseVnCnFromParts vn cn _ =
  case parseCnMood cn of
    Just mood ->
      case parseVnValence vn of
        Just val -> Success $ Just (val, MoodVal mood)
        Nothing -> Success Nothing
    Nothing ->
      case parseCnCaseScope cn of
        Just cs ->
          case parseVnValence vn of
            Just val -> Success $ Just (val, CaseScope cs)
            Nothing -> Success Nothing
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
  [ ("ai", PUN), ("au", ITR), ("ei", REP), ("eu", ITM)
  , ("ëu", RCT), ("ou", FRE), ("oi", FRG), ("iu", VAC), ("ui", FLC)
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
seriesAndForm v = listToMaybe
  [ (s + 1, f + 1)
  | (s, row) <- zip [0..] vowelFormTable
  , (f, vowel) <- zip [0..] row
  , vowelMatches v vowel
  ]

-- | Match vowels accounting for series 3 alternates (ia/uä etc.)
vowelMatches :: Text -> Text -> Bool
vowelMatches v form
  | "/" `T.isInfixOf` form = v `elem` T.splitOn "/" form
  | otherwise = v == form

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
    Nothing -> Penultimate  -- Default
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
      vowelPositions = [i | (i, c) <- zip [1..] chars, c `elem` ("aäeëiöoüu" ++ acuteVowels)]
      acutePositions = [i | (i, c) <- zip [1..] chars, c `elem` acuteVowels]
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
