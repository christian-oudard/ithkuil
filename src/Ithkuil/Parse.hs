{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Parsing
-- Parse text into grammatical structures
module Ithkuil.Parse
  ( ParsedFormative(..)
  , ParsedCa(..)
  , parseFormativeReal
  , parseSimpleWord
  , parseSlotII
  , parseSlotIV
  , parseConfiguration
  , parseCase
  , parseCa
  , splitConjuncts
  , isVowelChar
  , normalizeAccents
  , defaultCa
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (listToMaybe, isJust)
import Ithkuil.Phonology
import Ithkuil.Grammar

-- | Parse Vv vowel to Slot II (Stem + Version)
parseSlotII :: Text -> Maybe SlotII
parseSlotII v = listToMaybe
  [ slot | (slot, vv) <- slotIITable, vv == normalizeAccents v ]
  where
    slotIITable =
      [ ((S1, PRC), "a"),  ((S1, CPT), "ä")
      , ((S2, PRC), "e"),  ((S2, CPT), "i")
      , ((S3, PRC), "u"),  ((S3, CPT), "ü")
      , ((S0, PRC), "o"),  ((S0, CPT), "ö")
      ]

-- | Parse Vr vowel to Slot IV (Function + Specification + Context)
-- Uses the vowel form table: Series 1-4 map to EXS/FNC/RPS/AMG contexts
-- Forms 1-8 map to STA/DYN x BSC/CTE/CSV/OBJ
parseSlotIV :: Text -> Maybe SlotIV
parseSlotIV v = listToMaybe
  [ slot | (slot, vr) <- slotIVTable, vr == normalizeAccents v ]
  where
    slotIVTable = concatMap seriesEntries [(EXS, 1), (FNC, 2), (RPS, 3), (AMG, 4)]
    seriesEntries (ctx, s) =
      [ ((STA, BSC, ctx), vowelForm s 1)
      , ((STA, CTE, ctx), vowelForm s 2)
      , ((STA, CSV, ctx), vowelForm s 3)
      , ((STA, OBJ, ctx), vowelForm s 5)
      , ((DYN, OBJ, ctx), vowelForm s 6)
      , ((DYN, CSV, ctx), vowelForm s 7)
      , ((DYN, CTE, ctx), vowelForm s 8)
      , ((DYN, BSC, ctx), vowelForm s 9)
      ]

-- | Configuration consonant patterns (Ca1 component)
-- From official grammar ch03: UPX=null, DPX=s, MSS=t, MSC=k, etc.
configurationPatterns :: [(Configuration, Text)]
configurationPatterns =
  [ (UNI, "")
  , (DPX, "s")
  , (DSS, "c"),  (DSC, "ks"), (DSF, "ps")
  , (DDS, "ţs"), (DDC, "fs"), (DDF, "š")
  , (DFS, "č"),  (DFC, "kš"), (DFF, "pš")
  , (MSS, "t"),  (MSC, "k"),  (MSF, "p")
  , (MDS, "ţ"),  (MDC, "f"),  (MDF, "ç")
  , (MFS, "z"),  (MFC, "ž"),  (MFF, "ẓ")
  ]

-- | Parse Configuration from Ca consonant
parseConfiguration :: Text -> Maybe Configuration
parseConfiguration c = listToMaybe
  [ cfg | (cfg, pat) <- configurationPatterns, pat == c ]

-- | Extension patterns (Ca2 component)
extensionPatterns :: [(Extension, Text, Text)]  -- (ext, after-consonant, standalone/after-UPX)
extensionPatterns =
  [ (DEL, "",  "")
  , (PRX, "t", "d")
  , (ICP, "k", "g")
  , (ATV, "p", "b")
  , (GRA, "g", "gz")
  , (DPL, "b", "bz")
  ]

-- | Affiliation patterns (Ca3 component)
affiliationPatterns :: [(Affiliation, Text, Text)]  -- (aff, normal, standalone)
affiliationPatterns =
  [ (CSL, "",  "")
  , (ASO, "l", "nļ")
  , (COA, "r", "rļ")
  , (VAR, "ř", "ň")
  ]

-- | Perspective + Essence patterns (Ca4 component)
perspectiveEssencePatterns :: [((Perspective, Essence), Text, Text)]  -- ((persp,ess), standalone, after-consonant)
perspectiveEssencePatterns =
  [ ((M_, NRM), "l",  "")
  , ((G_, NRM), "r",  "r")
  , ((N_, NRM), "v",  "w")
  , ((A_, NRM), "z",  "y")
  , ((M_, RPV), "ř",  "ř")
  , ((G_, RPV), "tļ", "l")
  , ((N_, RPV), "lm", "m")
  , ((A_, RPV), "ln", "n")
  ]

-- | Parsed formative with all identified slots
data ParsedFormative = ParsedFormative
  { pfSlotII  :: SlotII          -- Stem + Version
  , pfRoot    :: Root            -- Cr (root consonants)
  , pfSlotIV  :: SlotIV          -- Function + Specification + Context
  , pfCa      :: [Text]          -- Ca complex (raw conjuncts)
  , pfCaParsed :: Maybe ParsedCa -- Parsed Ca values
  , pfCase    :: Maybe Case      -- Vc (case vowel, when penultimate/antepenultimate stress)
  , pfIllocVal :: Maybe (Illocution, Validation)  -- Vk (when ultimate stress)
  , pfStress  :: Stress          -- Detected stress pattern
  , pfConjuncts :: [Text]        -- Original conjunct split
  } deriving (Show, Eq)

-- | Parse a formative, handling both vowel-initial and consonant-initial words
-- Consonant-initial words have elided Vv (defaults to S1/PRC = "a")
-- Words starting with w/y + vowel are treated as vowel-initial (w/y is a glide)
parseFormativeReal :: Text -> Maybe ParsedFormative
parseFormativeReal word = do
  let lword = T.toLower word
      parts = splitConjuncts lword
      stress = detectStressSimple lword
  case parts of
    [] -> Nothing
    -- w/y glide before vowel: treat as vowel-initial (strip the glide)
    (cc:vv:rest) | isGlide cc -> parseVowelInitial stress (vv:rest)
    (first:_) ->
      if isConsonantCluster first
        then parseConsonantInitial stress parts  -- Elided Vv
        else parseVowelInitial stress parts      -- Normal Vv-Cr-Vr-Ca...
  where
    isGlide t = t == "w" || t == "y"

-- | Check if text is a consonant cluster (starts with consonant)
isConsonantCluster :: Text -> Bool
isConsonantCluster t = case T.uncons t of
  Nothing -> False
  Just (c, _) -> not (isVowelChar c)

-- | Parse consonant-initial word (elided Vv = default S1/PRC)
-- Tries Cr-Vr-Ca..., then falls back to CrCa-Vc/Vk (both Vv and Vr elided)
parseConsonantInitial :: Stress -> [Text] -> Maybe ParsedFormative
parseConsonantInitial stress parts = case parts of
  (cr:vr:rest) ->
    case rest of
      -- Minimal: Cr-Vc/Vk (both Vv and Vr elided)
      [] ->
        let (caseM, illocValM) = parseSlotIXSimple stress (Just vr)
        in Just ParsedFormative
          { pfSlotII = (S1, PRC)
          , pfRoot = Root cr
          , pfSlotIV = (STA, BSC, EXS)
          , pfCa = []
          , pfCaParsed = Just defaultCa
          , pfCase = caseM
          , pfIllocVal = illocValM
          , pfStress = stress
          , pfConjuncts = parts
          }
      -- Longer: Cr-Vr-Ca...-Vc/Vk
      _ ->
        case parseSlotIV vr of
          Just slotIV ->
            let (caRest, vcRest) = splitCaVc rest
                lastVowel = listToMaybe vcRest
                (caseM, illocValM) = parseSlotIXSimple stress lastVowel
                caConsonants = filter (not . T.null) $ filter isConsonantCluster caRest
                caParsedM = parseCa =<< listToMaybe caConsonants
            in Just ParsedFormative
              { pfSlotII = (S1, PRC)
              , pfRoot = Root cr
              , pfSlotIV = slotIV
              , pfCa = caRest
              , pfCaParsed = caParsedM
              , pfCase = caseM
              , pfIllocVal = illocValM
              , pfStress = stress
              , pfConjuncts = parts
              }
          -- Vr parse failed: try treating cr as merged CrCa with elided Vr
          Nothing -> do
            (root, caParsedM) <- splitCrCa cr
            let (caseM, illocValM) = parseSlotIXSimple stress (Just vr)
            Just ParsedFormative
              { pfSlotII = (S1, PRC)
              , pfRoot = Root root
              , pfSlotIV = (STA, BSC, EXS)
              , pfCa = [cr]
              , pfCaParsed = caParsedM
              , pfCase = caseM
              , pfIllocVal = illocValM
              , pfStress = stress
              , pfConjuncts = parts
              }
  _ -> Nothing

-- | Parse vowel-initial word (explicit Vv)
-- First tries Vv-Cr-Vr-Ca..., then falls back to Vv-CrCa-Vc/Vk (elided Vr)
parseVowelInitial :: Stress -> [Text] -> Maybe ParsedFormative
parseVowelInitial stress parts = case parts of
  (vv:cr:vr:rest) -> do
    slotII <- parseSlotII vv
    case rest of
      -- Minimal formative: Vv-Cr-Vc/Vk (Vr and Ca both elided to defaults)
      [] ->
        let (caseM, illocValM) = parseSlotIXSimple stress (Just vr)
        in Just ParsedFormative
          { pfSlotII = slotII
          , pfRoot = Root cr
          , pfSlotIV = (STA, BSC, EXS)
          , pfCa = []
          , pfCaParsed = Just defaultCa
          , pfCase = caseM
          , pfIllocVal = illocValM
          , pfStress = stress
          , pfConjuncts = parts
          }
      -- Longer formative: try Vr parse, fall back to elided Vr
      _ ->
        case parseSlotIV vr of
          Just slotIV ->
            let (caRest, vcRest) = splitCaVc rest
                lastVowel = listToMaybe vcRest
                (caseM, illocValM) = parseSlotIXSimple stress lastVowel
                caConsonants = filter (not . T.null) $ filter isConsonantCluster caRest
                caParsedM = parseCa =<< listToMaybe caConsonants
            in Just ParsedFormative
              { pfSlotII = slotII
              , pfRoot = Root cr
              , pfSlotIV = slotIV
              , pfCa = caRest
              , pfCaParsed = caParsedM
              , pfCase = caseM
              , pfIllocVal = illocValM
              , pfStress = stress
              , pfConjuncts = parts
              }
          -- If Vr parse fails, vr might actually be Vc/Vk with elided Vr
          Nothing -> tryElidedVr slotII cr vr stress parts
  -- Two elements: Vv + CrCa (elided Vr, no Vc/Vk)
  (vv:crca:[]) -> do
    slotII <- parseSlotII vv
    (root, caParsedM) <- splitCrCa crca
    Just ParsedFormative
      { pfSlotII = slotII
      , pfRoot = Root root
      , pfSlotIV = (STA, BSC, EXS)
      , pfCa = [crca]
      , pfCaParsed = caParsedM
      , pfCase = Nothing
      , pfIllocVal = Nothing
      , pfStress = stress
      , pfConjuncts = parts
      }
  _ -> Nothing

-- | Try parsing with elided Vr: the consonant cluster contains Cr+Ca merged
-- and the next vowel is Vc/Vk instead of Vr
tryElidedVr :: SlotII -> Text -> Text -> Stress -> [Text] -> Maybe ParsedFormative
tryElidedVr slotII crca vcvk stress parts = do
  (root, caParsedM) <- splitCrCa crca
  let (caseM, illocValM) = parseSlotIXSimple stress (Just vcvk)
  Just ParsedFormative
    { pfSlotII = slotII
    , pfRoot = Root root
    , pfSlotIV = (STA, BSC, EXS)  -- Default elided Vr
    , pfCa = [crca]
    , pfCaParsed = caParsedM
    , pfCase = caseM
    , pfIllocVal = illocValM
    , pfStress = stress
    , pfConjuncts = parts
    }

-- | Split a consonant cluster into Cr (root) + Ca (configuration complex)
-- Tries all split points, preferring the longest valid Ca from the right
splitCrCa :: Text -> Maybe (Text, Maybe ParsedCa)
splitCrCa cluster
  | T.null cluster = Nothing
  | otherwise =
    -- Try splits from right to left: longest Ca match first
    let len = T.length cluster
        splits = [ (T.take i cluster, T.drop i cluster) | i <- [1..len] ]
        validSplits = [ (cr, parseCa ca) | (cr, ca) <- splits
                       , not (T.null cr)
                       , not (T.null ca)
                       , isJust (parseCa ca) ]
    in case validSplits of
      ((cr, caM):_) -> Just (cr, caM)
      -- If no valid Ca split found, treat entire cluster as Cr with default Ca
      [] -> Just (cluster, Just defaultCa)

-- | Split remaining conjuncts into Ca complex and Vc
-- The last vowel cluster is typically Vc (case), rest is Ca
-- Handles glottal stop cases: a standalone ' in the Ca area shifts the case
-- to Relational/Affinitive series (cases 37-52)
splitCaVc :: [Text] -> ([Text], [Text])
splitCaVc parts =
  let isVowelCluster t = not (T.null t) && isVowelChar (T.head t)
      -- Merge V-'-V patterns into single Vc before splitting
      merged = mergeGlottalVowels parts
      revParts = reverse merged
      -- Skip trailing consonants (Slot X), then find the case vowel
      (_, afterC) = span (not . isVowelCluster) revParts
      (vcRev, caRev) = span isVowelCluster afterC
      -- Check if there's a glottal stop in the Ca area — if so, apply to Vc
      -- Glottal stop may be standalone ("'") or prefix of a consonant ("'l")
      hasGlottal = any containsGlottal (reverse caRev)
      caClean = map stripGlottal (filter (not . T.null) (map stripGlottal (reverse caRev)))
      containsGlottal t = "'" `T.isInfixOf` t
      stripGlottal t = T.filter (/= '\'') t
      vcFinal = if hasGlottal
        then map addGlottalToVc (reverse vcRev)
        else reverse vcRev
  in (caClean, vcFinal)
  where
    -- Add glottal stop to a Vc vowel for case groups 5-8
    -- Single vowels: "a" → "a'a", diphthongs: "ëi" → "ë'i"
    addGlottalToVc vc
      | T.length vc == 1 = vc <> "'" <> vc
      | T.length vc >= 2 = T.take 1 vc <> "'" <> T.drop 1 vc
      | otherwise = vc
    -- Merge vowel-glottal-vowel into single "V'V" conjunct
    mergeGlottalVowels (v1:"'":v2:rest)
      | not (T.null v1) && isVowelChar (T.head v1)
      , not (T.null v2) && isVowelChar (T.head v2)
      = mergeGlottalVowels ((v1 <> "'" <> v2) : rest)
    mergeGlottalVowels (x:rest) = x : mergeGlottalVowels rest
    mergeGlottalVowels [] = []

-- | Parse a simple word structure: Vv-Cr-Vr-Ca
-- Returns (SlotII, Root, SlotIV, partial SlotVI)
parseSimpleWord :: Text -> Maybe (SlotII, Root, SlotIV)
parseSimpleWord word = do
  pf <- parseFormativeReal word
  return (pfSlotII pf, pfRoot pf, pfSlotIV pf)

-- | Simple stress detection from acute accents
-- Detects ultimate/antepenultimate stress markers; defaults to penultimate
detectStressSimple :: Text -> Stress
detectStressSimple word
  | T.any isAcuteAccent word =
    let syllCount = length $ filter (\t -> not (T.null t) && isVowelChar (T.head t))
                           $ splitConjuncts word
        -- Find which syllable has the accent (1-indexed from start)
        chars = T.unpack word
        acutePos = length [c | c <- takeWhile (not . isAcuteAccent) chars, isVowelChar c] + 1
    in if acutePos == syllCount then Ultimate
       else if acutePos <= syllCount - 2 then Antepenultimate
       else Penultimate
  | otherwise = Penultimate
  where
    isAcuteAccent c = c `elem` ("áéíóú" :: String)

-- | Check if character is a vowel (includes accented vowels for all versions)
isVowelChar :: Char -> Bool
isVowelChar c = c `elem` ("aäeëiöoüuáéíóúàèìòùîâêôûǎěǐǒǔ" :: String)

-- | Parse Slot IX based on stress: Case (penultimate) or Vk (ultimate)
parseSlotIXSimple :: Stress -> Maybe Text -> (Maybe Case, Maybe (Illocution, Validation))
parseSlotIXSimple Ultimate (Just v) = (Nothing, parseVk v)
parseSlotIXSimple _ (Just v) = (parseCase v, Nothing)
parseSlotIXSimple _ Nothing = (Nothing, Nothing)

-- | Parse Vk (Illocution + Validation) from vowel
-- Series 1 = ASR + Validation (forms 1-9)
-- Series 2 = Other illocutions (forms by position)
parseVk :: Text -> Maybe (Illocution, Validation)
parseVk vk = case normalizeAccents vk of
  -- Series 1: ASR + Validation
  "a"  -> Just (ASR, OBS)
  "ä"  -> Just (ASR, REC)
  "e"  -> Just (ASR, PUP)
  "i"  -> Just (ASR, RPR)
  "ëi" -> Just (ASR, USP)
  "ö"  -> Just (ASR, IMA)
  "o"  -> Just (ASR, CVN)
  "ü"  -> Just (ASR, ITU)
  "u"  -> Just (ASR, INF)
  -- Series 2: Non-ASR illocutions
  "ai" -> Just (DIR, OBS)
  "au" -> Just (DEC, OBS)
  "ei" -> Just (IRG, OBS)
  "eu" -> Just (VER, OBS)
  "ou" -> Just (ADM, OBS)
  "oi" -> Just (POT, OBS)
  "iu" -> Just (HOR, OBS)
  "ui" -> Just (CNJ, OBS)
  _    -> Nothing

-- | Strip acute accents from vowels for parsing
normalizeAccents :: Text -> Text
normalizeAccents = T.map stripAccent
  where
    stripAccent 'á' = 'a'
    stripAccent 'é' = 'e'
    stripAccent 'í' = 'i'
    stripAccent 'ó' = 'o'
    stripAccent 'ú' = 'u'
    stripAccent c = c

-- | Split text into consonant/vowel conjuncts
splitConjuncts :: Text -> [Text]
splitConjuncts = filter (not . T.null) . T.groupBy sameType
  where
    sameType a b = isVowelChar a == isVowelChar b

-- | Case vowel patterns (Vc) - All 68 cases
casePatterns :: [(Case, Text)]
casePatterns =
  -- Transrelative (Series 1)
  [ (Transrelative THM, "a"), (Transrelative INS, "ä")
  , (Transrelative ABS, "e"), (Transrelative AFF, "i")
  , (Transrelative STM, "ëi"), (Transrelative EFF, "ö")
  , (Transrelative ERG, "o"), (Transrelative DAT, "ü")
  , (Transrelative IND, "u")
  -- Appositive (Series 2)
  , (Appositive POS, "ai"), (Appositive PRP, "au")
  , (Appositive GEN, "ei"), (Appositive ATT, "eu")
  , (Appositive PDC, "ëu"), (Appositive ITP, "ou")
  , (Appositive OGN, "oi"), (Appositive IDP, "iu")
  , (Appositive PAR, "ui")
  -- Associative (Series 3)
  , (Associative APL, "ia"), (Associative PUR, "iä")
  , (Associative TRA, "ie"), (Associative DFR, "ië")
  , (Associative CRS, "ëo"), (Associative TSP, "iö")
  , (Associative CMM, "io"), (Associative CMP, "iü")
  , (Associative CSD, "iu")
  -- Adverbial (Series 4)
  , (Adverbial FUN, "ua"), (Adverbial TFM, "uä")
  , (Adverbial CLA, "ue"), (Adverbial RSL, "uë")
  , (Adverbial CSM, "ëa"), (Adverbial CON, "uö")
  , (Adverbial AVR, "uo"), (Adverbial CVS, "uü")
  , (Adverbial SIT, "ui")
  -- Relational (Series 1 vowels + glottal stop)
  , (Relational PRN, "a'a"), (Relational DSP, "ä'ä")
  , (Relational COR, "e'e"), (Relational CPS, "i'i")
  , (Relational COM, "ë'i"), (Relational UTL, "ö'ö")
  , (Relational PRD, "o'o"), (Relational RLT, "u'u")
  -- Affinitive (Series 2 vowels + glottal stop)
  , (Affinitive ACT, "ai'i"), (Affinitive ASI, "au'u")
  , (Affinitive ESS, "ei'i"), (Affinitive TRM, "eu'u")
  , (Affinitive SEL, "ëu'u"), (Affinitive CFM, "ou'u")
  , (Affinitive DEP, "oi'i"), (Affinitive VOC, "ui'i")
  -- Spatio-Temporal I (Series 3 vowels + glottal stop)
  , (SpatioTemporal1 LOC, "ia'a"), (SpatioTemporal1 ATD, "iä'ä")
  , (SpatioTemporal1 ALL, "ie'e"), (SpatioTemporal1 ABL, "ië'ë")
  , (SpatioTemporal1 ORI, "ëo'o"), (SpatioTemporal1 IRL, "iö'ö")
  , (SpatioTemporal1 INV, "io'o"), (SpatioTemporal1 NAV, "iü'ü")
  -- Spatio-Temporal II (Series 4 vowels + glottal stop)
  , (SpatioTemporal2 CNR, "ua'a"), (SpatioTemporal2 ASS, "uä'ä")
  , (SpatioTemporal2 PER, "ue'e"), (SpatioTemporal2 PRO, "uë'ë")
  , (SpatioTemporal2 PCV, "ëa'a"), (SpatioTemporal2 PCR, "uö'ö")
  , (SpatioTemporal2 ELP, "uo'o"), (SpatioTemporal2 PLM, "uü'ü")
  ]

-- | Parse case from Vc vowel (normalizes accents)
parseCase :: Text -> Maybe Case
parseCase vc = listToMaybe
  [ c | (c, pattern) <- casePatterns, pattern == normalizeAccents vc ]

--------------------------------------------------------------------------------
-- Ca Complex Parsing (Slot VI)
--------------------------------------------------------------------------------

-- | Parsed Ca complex
data ParsedCa = ParsedCa
  { pcConfig      :: Configuration
  , pcAffiliation :: Affiliation
  , pcPerspective :: Perspective
  , pcExtension   :: Extension
  , pcEssence     :: Essence
  } deriving (Show, Eq)

-- | Default Ca values
defaultCa :: ParsedCa
defaultCa = ParsedCa UNI CSL M_ DEL NRM

-- | Parse Ca consonant cluster
-- Uses compositional decomposition: Configuration + Extension + Affiliation + Perspective/Essence
-- Falls back to common lookup table for standard forms
parseCa :: Text -> Maybe ParsedCa
parseCa ca = case lookup ca caLookupTable of
  Just pc -> Just pc
  Nothing -> tryCompositionalParse ca

-- | Try to parse Ca by decomposing into components
-- Works by trying all combinations and checking if they reconstruct the input
tryCompositionalParse :: Text -> Maybe ParsedCa
tryCompositionalParse ca = listToMaybe
  [ ParsedCa cfg aff persp ext ess
  | ((persp, ess), standalone, afterC) <- perspectiveEssencePatterns
  , let suffix = if T.null ca then standalone else afterC
  , not (T.null ca) || not (T.null standalone)
  , suffix `T.isSuffixOf` ca
  , let rest = T.dropEnd (T.length suffix) ca
  -- Match affiliation
  , (aff, affForm, _) <- affiliationPatterns
  , affForm `T.isSuffixOf` rest
  , let rest2 = T.dropEnd (T.length affForm) rest
  -- Match extension
  , (ext, extAfterC, _) <- extensionPatterns
  , extAfterC `T.isSuffixOf` rest2
  , let rest3 = T.dropEnd (T.length extAfterC) rest2
  -- Match configuration
  , (cfg, cfgForm) <- configurationPatterns
  , cfgForm == rest3
  ]

-- | Precomputed lookup table for common Ca forms
caLookupTable :: [(Text, ParsedCa)]
caLookupTable =
  -- Default: all-default Ca = "l"
  [ ("l",   ParsedCa UNI CSL M_ DEL NRM)
  -- Standalone perspective forms (UPX/CSL/DEL)
  , ("r",   ParsedCa UNI CSL G_ DEL NRM)
  , ("v",   ParsedCa UNI CSL N_ DEL NRM)
  , ("j",   ParsedCa UNI CSL A_ DEL NRM)
  , ("w",   ParsedCa UNI CSL N_ DEL NRM)
  , ("y",   ParsedCa UNI CSL A_ DEL NRM)
  -- Standalone RPV forms
  , ("tļ",  ParsedCa UNI CSL M_ DEL RPV)
  , ("ř",   ParsedCa UNI CSL M_ DEL RPV)
  -- Extension forms (UPX standalone: use voiced form)
  , ("d",   ParsedCa UNI CSL M_ PRX NRM)
  , ("g",   ParsedCa UNI CSL M_ ICP NRM)
  , ("b",   ParsedCa UNI CSL M_ ATV NRM)
  , ("gz",  ParsedCa UNI CSL M_ GRA NRM)
  , ("bz",  ParsedCa UNI CSL M_ DPL NRM)
  -- Affiliation standalone forms
  , ("nļ",  ParsedCa UNI ASO M_ DEL NRM)
  , ("rļ",  ParsedCa UNI COA M_ DEL NRM)
  , ("ň",   ParsedCa UNI VAR M_ DEL NRM)
  -- Configuration + default (CSL/DEL/M/NRM) with "l" suffix
  , ("tl",  ParsedCa MSS CSL M_ DEL NRM)
  , ("kl",  ParsedCa MSC CSL M_ DEL NRM)
  , ("pl",  ParsedCa MSF CSL M_ DEL NRM)
  , ("ţl",  ParsedCa MDS CSL M_ DEL NRM)
  , ("fl",  ParsedCa MDC CSL M_ DEL NRM)
  , ("çl",  ParsedCa MDF CSL M_ DEL NRM)
  , ("zl",  ParsedCa MFS CSL M_ DEL NRM)
  , ("žl",  ParsedCa MFC CSL M_ DEL NRM)
  , ("ẓl",  ParsedCa MFF CSL M_ DEL NRM)
  , ("cl",  ParsedCa DSS CSL M_ DEL NRM)
  , ("ksl", ParsedCa DSC CSL M_ DEL NRM)
  , ("psl", ParsedCa DSF CSL M_ DEL NRM)
  , ("sl",  ParsedCa DPX CSL M_ DEL NRM)
  -- Configuration + Agglomerative (G) perspective
  , ("tr",  ParsedCa MSS CSL G_ DEL NRM)
  , ("kr",  ParsedCa MSC CSL G_ DEL NRM)
  , ("pr",  ParsedCa MSF CSL G_ DEL NRM)
  , ("ţr",  ParsedCa MDS CSL G_ DEL NRM)
  , ("fr",  ParsedCa MDC CSL G_ DEL NRM)
  , ("çr",  ParsedCa MDF CSL G_ DEL NRM)
  , ("zr",  ParsedCa MFS CSL G_ DEL NRM)
  -- Configuration + Nomic (N) perspective
  , ("tw",  ParsedCa MSS CSL N_ DEL NRM)
  , ("kw",  ParsedCa MSC CSL N_ DEL NRM)
  , ("pw",  ParsedCa MSF CSL N_ DEL NRM)
  -- Configuration + Abstract (A) perspective
  , ("ty",  ParsedCa MSS CSL A_ DEL NRM)
  , ("ky",  ParsedCa MSC CSL A_ DEL NRM)
  , ("py",  ParsedCa MSF CSL A_ DEL NRM)
  ]
