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
  , parseCc
  , CcShortcut(..)
  , isSpecialVv
  , isRefRootVv
  , parseAffixVr
  , isGeminateCa
  , degeminateCa
  , detectStressSimple
  , vvSeries
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Applicative ((<|>))
import Data.Maybe (listToMaybe, isJust)
import Ithkuil.Phonology
import Ithkuil.Grammar
import Ithkuil.Allomorph (parseCaSlot)

-- | Parse Vv vowel to Slot II (Stem + Version)
-- Uses vowelFormLookup to resolve series/form (including Series 3 alternates)
-- Form→Stem: 1,2→S1; 3,4→S2; 9,8→S3; 7,6→S0
-- Form→Version: odd→PRC, even→CPT (form 5 = Cs-root special)
parseSlotII :: Text -> Maybe SlotII
parseSlotII v = do
  (_, form) <- vowelFormLookup (normalizeAccents v)
  formToStemVersion form
  where
    formToStemVersion f = case f of
      1 -> Just (S1, PRC); 2 -> Just (S1, CPT)
      3 -> Just (S2, PRC); 4 -> Just (S2, CPT)
      6 -> Just (S0, CPT); 7 -> Just (S0, PRC)
      8 -> Just (S3, CPT); 9 -> Just (S3, PRC)
      _ -> Nothing  -- form 5 = Cs-root special Vv

-- | Parse Vr vowel to Slot IV (Function + Specification + Context)
-- Series 1-4 → EXS/FNC/RPS/AMG; Forms map to Function x Specification
parseSlotIV :: Text -> Maybe SlotIV
parseSlotIV v = do
  (series, form) <- vowelFormLookup (normalizeAccents v)
  ctx <- case series of
    1 -> Just EXS; 2 -> Just FNC; 3 -> Just RPS; 4 -> Just AMG; _ -> Nothing
  funcSpec <- case form of
    1 -> Just (STA, BSC); 2 -> Just (STA, CTE); 3 -> Just (STA, CSV); 4 -> Just (STA, OBJ)
    6 -> Just (DYN, OBJ); 7 -> Just (DYN, CSV); 8 -> Just (DYN, CTE); 9 -> Just (DYN, BSC)
    _ -> Nothing  -- form 5 = Cs-root special
  Just (fst funcSpec, snd funcSpec, ctx)

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


-- | Parsed formative with all identified slots
data ParsedFormative = ParsedFormative
  { pfConcatenation :: Maybe ConcatenationStatus  -- Cc concatenation type
  , pfSlotII  :: SlotII          -- Stem + Version
  , pfRoot    :: Root            -- Cr (root consonants)
  , pfSlotIV  :: SlotIV          -- Function + Specification + Context
  , pfCa      :: [Text]          -- Ca complex (raw conjuncts)
  , pfCaParsed :: Maybe ParsedCa -- Parsed Ca values
  , pfSlotVIII :: Maybe SlotVIII -- VnCn (valence/phase/aspect + mood/case-scope)
  , pfCase    :: Maybe Case      -- Vc (case vowel, when penultimate/antepenultimate stress)
  , pfIllocVal :: Maybe (Illocution, Validation)  -- Vk (when ultimate stress)
  , pfSlotV   :: [(Text, Text)]   -- Slot V CsVx affix pairs (reversed order)
  , pfStress  :: Stress          -- Detected stress pattern
  , pfConjuncts :: [Text]        -- Original conjunct split
  , pfCsRootDegree :: Maybe Int  -- Cs-root degree (Nothing for normal roots)
  , pfSentenceStarter :: Bool    -- True when word had ç sentence prefix
  , pfVvSeries :: Int            -- Vv vowel series (1-4); Series 2-4 carry implicit affix
  , pfHasShortcut :: Bool        -- True when Cc shortcut (w/y) was used (no implicit affix)
  , pfSlotVMarker :: Bool        -- True when glottal stop signals 2+ Slot V affixes
  } deriving (Show, Eq)

-- | Cc shortcut type (w or y prefix that adds Ca values)
data CcShortcut = ShortcutW | ShortcutY deriving (Show, Eq)

-- | Parse Cc consonant for concatenation type and shortcut
-- CC consonants: h/hl/hm (Type1), hw/hr/hn (Type2), w/y (shortcut only)
-- Some carry both: hl=T1+W, hm=T1+Y, hr=T2+W, hn=T2+Y
parseCc :: Text -> (Maybe ConcatenationStatus, Maybe CcShortcut)
parseCc cc = (concat_, shortcut)
  where
    concat_ = case cc of
      "h"  -> Just Type1
      "hl" -> Just Type1
      "hm" -> Just Type1
      "hw" -> Just Type2
      "hr" -> Just Type2
      "hn" -> Just Type2
      _    -> Nothing
    shortcut = case cc of
      "w"  -> Just ShortcutW
      "hl" -> Just ShortcutW
      "hr" -> Just ShortcutW
      "y"  -> Just ShortcutY
      "hm" -> Just ShortcutY
      "hn" -> Just ShortcutY
      _    -> Nothing

-- | Resolve Ca from Cc shortcut + Vv series
shortcutCa :: CcShortcut -> Int -> ParsedCa
shortcutCa ShortcutW 1 = defaultCa
shortcutCa ShortcutW 2 = ParsedCa UNI CSL G_ DEL NRM
shortcutCa ShortcutW 3 = ParsedCa UNI CSL N_ DEL NRM
shortcutCa ShortcutW 4 = ParsedCa UNI CSL G_ DEL RPV
shortcutCa ShortcutY 1 = ParsedCa UNI CSL M_ PRX NRM
shortcutCa ShortcutY 2 = ParsedCa UNI CSL M_ DEL RPV
shortcutCa ShortcutY 3 = ParsedCa UNI CSL A_ DEL NRM
shortcutCa ShortcutY 4 = ParsedCa UNI CSL M_ PRX RPV
shortcutCa _ _ = defaultCa

-- | Get Vv series number (1-4) from the vowel form
vvSeries :: Text -> Int
vvSeries vv = case vowelFormLookup (normalizeAccents vv) of
  Just (s, _) -> s
  Nothing     -> 1

-- | Check if a Vv value is a special marker (Cs-root or reference-root)
-- ëi/eë/ëu/oë = Cs-root (root consonant is an affix Cs)
-- ae/ea = reference-root (root consonant is a referential C1)
isSpecialVv :: Text -> Bool
isSpecialVv v = normalizeAccents v `elem` ["ëi", "eë", "ëu", "oë", "ae", "ea"]

-- | Check if a Vv value is a reference-root marker (ae/ea)
isRefRootVv :: Text -> Bool
isRefRootVv v = normalizeAccents v `elem` ["ae", "ea"]

-- | Parse special Vv for Cs-root/reference-root formatives
-- Returns (Version, Maybe Function)
-- For reference roots (ae/ea), Function is Nothing
parseSpecialVv :: Text -> Maybe (Version, Maybe Function)
parseSpecialVv v = case normalizeAccents v of
  "ëi" -> Just (PRC, Just STA)
  "eë" -> Just (PRC, Just DYN)
  "ëu" -> Just (CPT, Just STA)
  "oë" -> Just (CPT, Just DYN)
  "ae" -> Just (PRC, Nothing)
  "ea" -> Just (CPT, Nothing)
  _    -> Nothing

-- | Parse Vr for Cs-root formatives: degree (form) + context (series)
-- Series 1-4 maps to EXS/FNC/RPS/AMG; Form 0-9 maps to degree
parseAffixVr :: Text -> Maybe (Int, Context)
parseAffixVr vr =
  let nv = normalizeAccents vr
  in case nv of
    -- Degree-0 forms
    "ae" -> Just (0, EXS)
    "ea" -> Just (0, FNC)
    "üo" -> Just (0, RPS)
    "üö" -> Just (0, AMG)
    _ -> do
      (series, form) <- vowelFormLookup nv
      let ctx = case series of
            1 -> EXS; 2 -> FNC; 3 -> RPS; 4 -> AMG; _ -> EXS
      Just (form, ctx)

-- | Parse a formative, handling both vowel-initial and consonant-initial words
-- Consonant-initial words have elided Vv (defaults to S1/PRC = "a")
-- Words starting with w/y + vowel are treated as vowel-initial (w/y is a Cc shortcut)
-- Words starting with h/hl/hm/hw/hr/hn are concatenated formatives
parseFormativeReal :: Text -> Maybe ParsedFormative
parseFormativeReal word = do
  let lword = T.toLower word
      -- Detect ç sentence prefix
      hasSentencePrefix = case T.uncons lword of
        Just ('ç', _) -> True
        _ -> False
      -- Strip ç sentence prefix (çë, ça, çw, çy, çç patterns)
      -- çë uses "ë" as default Vv marker — strip both ç and ë
      stripped = case T.uncons lword of
        Just ('ç', rest) -> case T.uncons rest of
          Just ('ë', rest2) | not (T.null rest2) -> rest2  -- çë = strip prefix + default Vv
          Just ('ç', rest2) -> "y" <> rest2  -- çç = sentence prefix + y shortcut (PRX)
          _ | not (T.null rest) -> rest
          _ -> lword
        _ -> lword
      parts = splitConjuncts stripped
      stress = detectStressSimple stripped
      markSentence pf = pf { pfSentenceStarter = hasSentencePrefix }
  case parts of
    [] -> Nothing
    -- Check for Cc consonant (concatenation/shortcut): w, y, h, hl, hm, hw, hr, hn
    (cc:rest) | not (null rest) ->
      let (concatM, scM) = parseCc cc
      in case (concatM, scM) of
        -- Pure shortcut (w or y): parse with shortcut Ca
        (Nothing, Just sc) -> do
          pf <- parseVowelInitialWithShortcut sc False stress rest
          return (markSentence pf)
        -- Concatenation with shortcut (hl, hm, hr, hn): parse with shortcut Ca + concat
        (Just ct, Just sc) -> do
          pf <- parseVowelInitialWithShortcut sc True stress rest
          return (markSentence pf) { pfConcatenation = Just ct }
        -- Concatenation without shortcut (h, hw): parse rest normally
        (Just ct, Nothing) -> do
          pf <- parseRestAsFormative stress rest
          return (markSentence pf) { pfConcatenation = Just ct }
        -- No Cc match: normal parsing
        (Nothing, Nothing) ->
          fmap markSentence $
          if isConsonantCluster cc
            then parseConsonantInitial stress parts  -- Elided Vv
            else parseVowelInitial stress parts      -- Normal Vv-Cr-Vr-Ca...
    (first:_) ->
      fmap markSentence $
      if isConsonantCluster first
        then parseConsonantInitial stress parts  -- Elided Vv
        else parseVowelInitial stress parts      -- Normal Vv-Cr-Vr-Ca...

-- | Parse remaining conjuncts as a formative after stripping Cc
parseRestAsFormative :: Stress -> [Text] -> Maybe ParsedFormative
parseRestAsFormative stress parts = case parts of
  [] -> Nothing
  (first:_)
    | not (T.null first) && isVowelChar (T.head first) ->
        parseVowelInitial stress parts
    | otherwise ->
        parseConsonantInitial stress parts

-- | Parse vowel-initial word with Cc shortcut (w or y prefix)
-- Shortcuts elide Slots IV (Vr) and VI (Ca), setting Ca from the shortcut table
-- After Cr, remaining conjuncts are: [VxCs Slot VII affixes...] + Vc/Vk
parseVowelInitialWithShortcut :: CcShortcut -> Bool -> Stress -> [Text] -> Maybe ParsedFormative
parseVowelInitialWithShortcut sc inConcat stress parts = case parts of
  (vv:cr0:rest) -> do
    let (cr, slotVFilled0) = stripSlotVMarker cr0
    slotII <- parseSlotII vv
    let series = vvSeries vv
        scCa = shortcutCa sc series
        -- First merge glottal-vowel patterns (a ' a -> a'a), then process glottals
        merged0 = mergeGlottalVowels rest
        -- Process glottal stops: strip from vowels (a'a -> a), track positions
        (merged, hasGlottalInAffix) = processGlottalStops merged0
        slotVFilled = slotVFilled0
        (affixParts, lastVowel) = splitAfterShortcut merged
        -- Concatenated formatives always parse as case (never Illocution+Validation)
        (caseM, illocValM) = if inConcat
          then (parseConcatSlotIX stress lastVowel, Nothing)
          else parseSlotIXSimple stress lastVowel
        -- For shortcut formatives with VxCs-region glottal, split affixes:
        -- All affixes before glottal boundary go to slot V (CsVx order)
        -- Remaining go to slot VII (stored in pfCa for extractAffixes)
        -- When no glottal in affix region, all go to slot VII
        (slotVPairs, slotVIIParts) = if hasGlottalInAffix
          then splitSlotVFromVII affixParts
          else ([], affixParts)
        caWithAffixes = if null slotVIIParts then [] else "" : slotVIIParts
    Just ParsedFormative
      { pfConcatenation = Nothing
      , pfSlotII = slotII
      , pfRoot = Root cr
      , pfSlotIV = (STA, BSC, EXS)  -- Shortcuts elide Vr to default
      , pfCa = caWithAffixes        -- Slot VII VxCs pairs
      , pfCaParsed = Just scCa
      , pfSlotVIII = Nothing
      , pfCase = caseM
      , pfIllocVal = illocValM
      , pfSlotV = slotVPairs         -- Slot V CsVx pairs (from glottal boundary)
      , pfStress = stress
      , pfConjuncts = parts
      , pfCsRootDegree = Nothing
      , pfSentenceStarter = False
      , pfVvSeries = series
      , pfHasShortcut = True
      , pfSlotVMarker = slotVFilled
      }
  _ -> Nothing

-- | Process glottal stops in conjunct list for shortcut formatives
-- Strips glottal stops from vowels (a'a -> a), returns whether any glottal was found
-- This handles Kotlin's glottal stop movement: a moved glottal in VxCs marks slot V boundary
processGlottalStops :: [Text] -> ([Text], Bool)
processGlottalStops parts =
  let process [] = ([], False)
      process (t:ts) =
        let hasGlottal = "'" `T.isInfixOf` t
            cleaned = unglottalizeVowel t
            (rest', anyGlottal) = process ts
        in (cleaned : rest', hasGlottal || anyGlottal)
  in process parts

-- | Remove glottal stop from vowel form (Kotlin: unglottalizeVowel)
-- "a'a" -> "a", "ö'e" -> "öe", "a" -> "a"
unglottalizeVowel :: Text -> Text
unglottalizeVowel v =
  let stripped = T.filter (/= '\'') v
  in if T.length stripped == 2 && T.head stripped == T.last stripped
     then T.take 1 stripped
     else stripped

-- | Split VxCs affix parts at glottal boundary into (slot V, slot VII)
-- For shortcut formatives, a glottal stop in the VxCs region marks the end of slot V
-- Takes alternating [V, C, V, C, ...] parts
-- The first VxCs pair goes to slot V; remaining go to slot VII
-- Slot V stores in CsVx order (reversed from the VxCs input)
splitSlotVFromVII :: [Text] -> ([(Text, Text)], [Text])
splitSlotVFromVII (vx:cs:remaining) = ([(cs, vx)], remaining)
splitSlotVFromVII _ = ([], [])

-- | Split conjuncts after shortcut Cr into (affix parts, last vowel)
-- Takes [V, C, V, C, ..., V] and returns (VxCs affix pairs as [V,C,...], last V)
-- Special case: when parts end in a consonant (e.g. [V, C] with no final vowel),
-- treat the entire sequence as affix/VnCn parts with elided Vc (default THM),
-- since standalone shortcut formatives normally end in a vowel (Vc).
splitAfterShortcut :: [Text] -> ([Text], Maybe Text)
splitAfterShortcut [] = ([], Nothing)
splitAfterShortcut parts =
  let isVowelCluster t = not (T.null t) && isVowelChar (T.head t)
      revParts = reverse parts
      -- Find the last vowel cluster (Vc/Vk)
      (trailingC, afterC) = span (not . isVowelCluster) revParts
  in case afterC of
    (vc:rest)
      -- Word ends in consonant with only one vowel: likely V+C affix/VnCn, not Vc+trailing
      | null rest && not (null trailingC) -> (parts, Nothing)
      | otherwise ->
          -- rest (reversed) = affix V-C pairs before Vc
          -- trailingC = trailing consonants after Vc (unusual but possible)
          let affixParts = reverse rest ++ reverse trailingC
          in (affixParts, Just vc)
    [] -> (parts, Nothing)

-- | Merge vowel-glottal-vowel into single "V'V" conjunct
mergeGlottalVowels :: [Text] -> [Text]
mergeGlottalVowels (v1:"'":v2:rest)
  | not (T.null v1) && isVowelChar (T.head v1)
  , not (T.null v2) && isVowelChar (T.head v2)
  = mergeGlottalVowels ((v1 <> "'" <> v2) : rest)
mergeGlottalVowels (x:rest) = x : mergeGlottalVowels rest
mergeGlottalVowels [] = []

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
          { pfConcatenation = Nothing
          , pfSlotII = (S1, PRC)
          , pfRoot = Root cr
          , pfSlotIV = (STA, BSC, EXS)
          , pfCa = []
          , pfCaParsed = Just defaultCa
          , pfSlotVIII = Nothing
          , pfCase = caseM
          , pfIllocVal = illocValM
          , pfSlotV = []
          , pfStress = stress
          , pfConjuncts = parts
          , pfCsRootDegree = Nothing
          , pfSentenceStarter = False
          , pfVvSeries = 1  -- Elided Vv defaults to Series 1
          , pfHasShortcut = False
          , pfSlotVMarker = False
          }
      -- Longer: Cr-Vr-Ca...-Vc/Vk
      _ ->
        case parseSlotIV vr of
          Just slotIV ->
            let (slotVAfx, caRest, vcRest) = parseAfterVr rest
                lastVowel = listToMaybe vcRest
                (caseM, illocValM) = parseSlotIXSimple stress lastVowel
                caConsonants = filter (not . T.null) $ filter isConsonantCluster caRest
                caParsedM = parseCa =<< listToMaybe caConsonants
            in Just ParsedFormative
              { pfConcatenation = Nothing
              , pfSlotII = (S1, PRC)
              , pfRoot = Root cr
              , pfSlotIV = slotIV
              , pfCa = caRest
              , pfCaParsed = caParsedM
              , pfSlotVIII = Nothing
              , pfCase = caseM
              , pfIllocVal = illocValM
              , pfSlotV = slotVAfx
              , pfStress = stress
              , pfConjuncts = parts
              , pfCsRootDegree = Nothing
              , pfSentenceStarter = False
              , pfVvSeries = 1  -- Elided Vv defaults to Series 1
              , pfHasShortcut = False
              , pfSlotVMarker = False
              }
          -- Vr parse failed: try treating cr as merged CrCa with elided Vr
          Nothing -> do
            (root, caParsedM) <- splitCrCa cr
            let (caseM, illocValM) = parseSlotIXSimple stress (Just vr)
            Just ParsedFormative
              { pfConcatenation = Nothing
              , pfSlotII = (S1, PRC)
              , pfRoot = Root root
              , pfSlotIV = (STA, BSC, EXS)
              , pfCa = [cr]
              , pfCaParsed = caParsedM
              , pfSlotVIII = Nothing
              , pfCase = caseM
              , pfIllocVal = illocValM
              , pfSlotV = []
              , pfStress = stress
              , pfConjuncts = parts
              , pfCsRootDegree = Nothing
              , pfSentenceStarter = False
              , pfVvSeries = 1  -- Elided Vv defaults to Series 1
              , pfHasShortcut = False
              , pfSlotVMarker = False
              }
  _ -> Nothing

-- | Strip Slot V filled marker (' prefix on Cr)
-- When Vv is followed by glottal stop, it signals 2+ Slot V affixes
stripSlotVMarker :: Text -> (Text, Bool)
stripSlotVMarker cr = case T.uncons cr of
  Just ('\'', rest) | not (T.null rest) -> (rest, True)
  _ -> (cr, False)

-- | Parse vowel-initial word (explicit Vv)
-- First checks for special Cs-root Vv values, then tries normal parsing
parseVowelInitial :: Stress -> [Text] -> Maybe ParsedFormative
parseVowelInitial stress parts = case parts of
  (vv:cr0:vr:rest)
    | isSpecialVv vv -> parseCsRootFormative stress vv cr0 vr rest
    | otherwise -> do
    let (cr, slotVFilled) = stripSlotVMarker cr0
    slotII <- parseSlotII vv
    let series = vvSeries vv
    case rest of
      -- Minimal formative: Vv-Cr-Vc/Vk (Vr and Ca both elided to defaults)
      [] ->
        let (caseM, illocValM) = parseSlotIXSimple stress (Just vr)
        in Just ParsedFormative
          { pfConcatenation = Nothing
          , pfSlotII = slotII
          , pfRoot = Root cr
          , pfSlotIV = (STA, BSC, EXS)
          , pfCa = []
          , pfCaParsed = Just defaultCa
          , pfSlotVIII = Nothing
          , pfCase = caseM
          , pfIllocVal = illocValM
          , pfSlotV = []
          , pfStress = stress
          , pfConjuncts = parts
          , pfCsRootDegree = Nothing
          , pfSentenceStarter = False
          , pfVvSeries = series
          , pfHasShortcut = False
          , pfSlotVMarker = slotVFilled
          }
      -- Longer formative: try Vr parse, fall back to elided Vr
      _ ->
        case parseSlotIV vr of
          Just slotIV ->
            let (slotVAfx, caRest, vcRest) = parseAfterVr rest
                lastVowel = listToMaybe vcRest
                (caseM, illocValM) = parseSlotIXSimple stress lastVowel
                caConsonants = filter (not . T.null) $ filter isConsonantCluster caRest
                caParsedM = parseCa =<< listToMaybe caConsonants
            in Just ParsedFormative
              { pfConcatenation = Nothing
              , pfSlotII = slotII
              , pfRoot = Root cr
              , pfSlotIV = slotIV
              , pfCa = caRest
              , pfCaParsed = caParsedM
              , pfSlotVIII = Nothing
              , pfCase = caseM
              , pfIllocVal = illocValM
              , pfSlotV = slotVAfx
              , pfStress = stress
              , pfConjuncts = parts
              , pfCsRootDegree = Nothing
              , pfSentenceStarter = False
              , pfVvSeries = series
              , pfHasShortcut = False
              , pfSlotVMarker = slotVFilled
              }
          -- If Vr parse fails, vr might actually be Vc/Vk with elided Vr
          Nothing -> tryElidedVr slotII cr vr series stress parts slotVFilled
  -- Two elements: Vv + CrCa (elided Vr, no Vc/Vk)
  (vv:crca:[]) -> do
    slotII <- parseSlotII vv
    (root, caParsedM) <- splitCrCa crca
    Just ParsedFormative
      { pfConcatenation = Nothing
      , pfSlotII = slotII
      , pfRoot = Root root
      , pfSlotIV = (STA, BSC, EXS)
      , pfCa = [crca]
      , pfCaParsed = caParsedM
      , pfSlotVIII = Nothing
      , pfCase = Nothing
      , pfIllocVal = Nothing
      , pfSlotV = []
      , pfStress = stress
      , pfConjuncts = parts
      , pfCsRootDegree = Nothing
      , pfSentenceStarter = False
      , pfVvSeries = vvSeries vv
      , pfHasShortcut = False
      , pfSlotVMarker = False
      }
  _ -> Nothing

-- | Parse a Cs-root formative (special Vv: ëi, eë, ëu, oë)
-- or a reference-root formative (special Vv: ae, ea)
-- For Cs-root: the "root" consonant is a Cs affix form; Vr encodes degree + context
-- For ref-root: the "root" consonant is a referential C1; Vr encodes case
parseCsRootFormative :: Stress -> Text -> Text -> Text -> [Text] -> Maybe ParsedFormative
parseCsRootFormative stress vv cs vr rest = do
  (version, mFunc) <- parseSpecialVv vv
  case mFunc of
    Just func -> do
      -- Cs-root: Vr = degree + context
      (degree, ctx) <- parseAffixVr vr
      let (slotVAfx, caRest, vcRest) = parseAfterVr rest
          lastVowel = listToMaybe vcRest
          (caseM, illocValM) = parseSlotIXSimple stress lastVowel
          caConsonants = filter (not . T.null) $ filter isConsonantCluster caRest
          caParsedM = parseCa =<< listToMaybe caConsonants
      Just ParsedFormative
        { pfConcatenation = Nothing
        , pfSlotII = (S1, version)
        , pfRoot = Root cs
        , pfSlotIV = (func, BSC, ctx)
        , pfCa = caRest
        , pfCaParsed = caParsedM
        , pfSlotVIII = Nothing
        , pfCase = caseM
        , pfIllocVal = illocValM
        , pfSlotV = slotVAfx
        , pfStress = stress
        , pfConjuncts = vv : cs : vr : rest
        , pfCsRootDegree = Just degree
        , pfSentenceStarter = False
        , pfVvSeries = 1  -- Special Vv, no implicit affix
        , pfHasShortcut = False
        , pfSlotVMarker = False
        }
    Nothing -> do
      -- Reference-root: Vr = normal Slot IV (function/spec/context)
      -- The root consonant is a referential C1
      let slotIV = case parseSlotIV vr of
            Just s -> s
            Nothing -> (STA, BSC, EXS)
          (slotVAfx, caRest, vcRest) = parseAfterVr rest
          lastVowel = listToMaybe vcRest
          (caseM, illocValM) = parseSlotIXSimple stress lastVowel
          caConsonants = filter (not . T.null) $ filter isConsonantCluster caRest
          caParsedM = parseCa =<< listToMaybe caConsonants
      Just ParsedFormative
        { pfConcatenation = Nothing
        , pfSlotII = (S1, version)
        , pfRoot = Root cs  -- referential C1 stored as root
        , pfSlotIV = slotIV
        , pfCa = caRest
        , pfCaParsed = caParsedM
        , pfSlotVIII = Nothing
        , pfCase = caseM
        , pfIllocVal = illocValM
        , pfSlotV = slotVAfx
        , pfStress = stress
        , pfConjuncts = vv : cs : vr : rest
        , pfCsRootDegree = Nothing  -- Not a Cs-root
        , pfSentenceStarter = False
        , pfVvSeries = 1  -- Special Vv, no implicit affix
        , pfHasShortcut = False
        , pfSlotVMarker = False
        }

-- | Try parsing with elided Vr: the consonant cluster contains Cr+Ca merged
-- and the next vowel is Vc/Vk instead of Vr
tryElidedVr :: SlotII -> Text -> Text -> Int -> Stress -> [Text] -> Bool -> Maybe ParsedFormative
tryElidedVr slotII crca vcvk vvSer stress parts slotVFilled' = do
  (root, caParsedM) <- splitCrCa crca
  let (caseM, illocValM) = parseSlotIXSimple stress (Just vcvk)
  Just ParsedFormative
    { pfConcatenation = Nothing
    , pfSlotII = slotII
    , pfRoot = Root root
    , pfSlotIV = (STA, BSC, EXS)  -- Default elided Vr
    , pfCa = [crca]
    , pfCaParsed = caParsedM
    , pfSlotVIII = Nothing
    , pfCase = caseM
    , pfIllocVal = illocValM
    , pfSlotV = []
    , pfStress = stress
    , pfConjuncts = parts
    , pfCsRootDegree = Nothing
    , pfSentenceStarter = False
    , pfVvSeries = vvSer
    , pfHasShortcut = False
    , pfSlotVMarker = slotVFilled'
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

-- | Check if a consonant cluster is a geminated Ca form
-- Gemination detected by adjacent duplicate characters or special allomorphs
isGeminateCa :: Text -> Bool
isGeminateCa t
  | T.null t = False
  | t `elem` caDegeminations = True
  | otherwise = hasAdjacentDuplicate (T.unpack t)
  where
    hasAdjacentDuplicate [] = False
    hasAdjacentDuplicate [_] = False
    hasAdjacentDuplicate (a:b:rest) = a == b || hasAdjacentDuplicate (b:rest)
    caDegeminations = map fst caDegemMap

-- | Special gemination allomorphs that don't follow the simple reduplication rule
caDegemMap :: [(Text, Text)]
caDegemMap =
  [ ("jjn", "dn"), ("jjm", "dm")
  , ("gžžn", "gn"), ("gžžm", "gm"), ("bžžn", "bn"), ("bžžm", "bm")
  , ("ḑḑn", "tn"), ("ḑḑm", "tm"), ("xxn", "kn"), ("xxm", "km")
  , ("vvn", "pn"), ("vmm", "pm")
  , ("ddv", "tp"), ("ḑvv", "tk"), ("ggv", "kp"), ("ggḑ", "kt")
  , ("bbv", "pk"), ("bbḑ", "pt")
  ]

-- | Remove gemination from Ca consonant cluster
degeminateCa :: Text -> Text
degeminateCa t =
  -- First check allomorph exceptions
  case lookup t caDegemMap of
    Just degemmed -> degemmed
    Nothing ->
      -- Remove adjacent duplicates
      let chars = T.unpack t
          dedup [] = []
          dedup [c] = [c]
          dedup (a:b:rest)
            | a == b = a : dedup rest
            | otherwise = a : dedup (b:rest)
      in T.pack (dedup chars)

-- | Try to extract Slot V CsVx affixes by scanning for geminated Ca
-- Returns Just (slotV pairs, restructured rest with degeminated Ca) if found
-- Returns Nothing if no gemination detected (no Slot V affixes)
trySlotV :: [Text] -> Maybe ([(Text, Text)], [Text])
trySlotV parts = go parts []
  where
    isVowelCluster t = not (T.null t) && isVowelChar (T.head t)
    go (c:rest) acc
      | isConsonantCluster c && isGeminateCa c =
          -- Found geminated Ca. acc contains (cs,vx) pairs collected so far.
          -- But if no Slot V affixes collected yet and the form is a valid Ca
          -- on its own, prefer non-geminated reading (e.g. "sstl" is a natural Ca)
          if null acc && isJust (parseCaSlot c)
          then Nothing  -- not a geminate, just a natural Ca with repeated consonants
          else Just (reverse acc, degeminateCa c : rest)
      | isConsonantCluster c = case rest of
          (v:rest')
            | isVowelCluster v -> go rest' ((c, v) : acc)
            | otherwise -> Nothing  -- consonant followed by consonant, abort
          [] -> Nothing  -- no vowel partner, abort
      | otherwise = Nothing  -- hit a vowel when expecting consonant, abort
    go [] _ = Nothing  -- reached end without finding geminated Ca

-- | Parse everything after Vr: try Slot V extraction, then Ca + Slot VII + Vc
-- Returns (slotV affixes, caRest, vcRest)
parseAfterVr :: [Text] -> ([(Text, Text)], [Text], [Text])
parseAfterVr rest =
  case trySlotV rest of
    Just (sv, ca:afterCa)
      | null afterCa -> (sv, [ca], [])  -- Only Ca, no Vc/Vk
      | otherwise ->
          -- afterCa = [Slot VII VxCs...] [VnCn] [Vc/Vk]
          -- Prepend Ca to let splitCaVc handle the rest normally
          let (caRest, vcRest) = splitCaVc (ca : afterCa)
          in (sv, caRest, vcRest)
    Just (sv, []) -> (sv, [], [])
    Nothing ->
      let (caRest, vcRest) = splitCaVc rest
      in ([], caRest, vcRest)

-- | Split remaining conjuncts into Ca complex and Vc
-- The last vowel cluster is typically Vc (case), rest is Ca
-- Handles glottal stop cases: a standalone ' in the Ca area shifts the case
-- to Relational/Affinitive series (cases 37-52)
-- Special case: when the word ends in a consonant and the last vowel is the
-- ONLY vowel after Ca, treat V+C as a Slot VII affix (not Vc + trailing),
-- since formatives normally end in a vowel and the V+C is more likely VxCs.
splitCaVc :: [Text] -> ([Text], [Text])
splitCaVc parts =
  let isVowelCluster t = not (T.null t) && isVowelChar (T.head t)
      -- Merge V-'-V patterns into single Vc before splitting
      merged = mergeGlottalVowels parts
      revParts = reverse merged
      -- Skip trailing consonants (Slot X), then find the case vowel
      (trailingC, afterC) = span (not . isVowelCluster) revParts
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
      -- When word ends in consonant and the "Vc" is the only vowel after Ca,
      -- the V+C pair is more likely a Slot VII affix than Vc + trailing consonant
      caHasVowels = any isVowelCluster (reverse caRev)
  in if not (null trailingC) && not (null vcRev) && not caHasVowels
     then (caClean ++ vcFinal ++ reverse trailingC, [])
     else (caClean, vcFinal)
  where
    -- Add glottal stop to a Vc vowel for case groups 5-8
    -- Single vowels: "a" → "a'a", diphthongs: "ëi" → "ë'i"
    addGlottalToVc vc
      | T.length vc == 1 = vc <> "'" <> vc
      | T.length vc >= 2 = T.take 1 vc <> "'" <> T.drop 1 vc
      | otherwise = vc
    -- Uses top-level mergeGlottalVowels

-- | Parse a simple word structure: Vv-Cr-Vr-Ca
-- Returns (SlotII, Root, SlotIV, partial SlotVI)
parseSimpleWord :: Text -> Maybe (SlotII, Root, SlotIV)
parseSimpleWord word = do
  pf <- parseFormativeReal word
  return (pfSlotII pf, pfRoot pf, pfSlotIV pf)

-- | Simple stress detection from accent marks
-- Acute (á) and circumflex (â) both mark the stressed syllable
-- Detects ultimate/antepenultimate stress markers; defaults to penultimate
detectStressSimple :: Text -> Stress
detectStressSimple word =
  let conjs = splitConjuncts word
      syllables = filter (\t -> not (T.null t) && isVowelChar (T.head t)) conjs
      syllCount = length syllables
  in if syllCount <= 1 && not (T.any isStressedVowel word)
     then Monosyllabic
     else if T.any isStressedVowel word
       then let hasStress t = T.any isStressedVowel t
                stressPos = length (takeWhile (not . hasStress) syllables) + 1
            in if stressPos == syllCount then Ultimate
               else if stressPos <= syllCount - 2 then Antepenultimate
               else Penultimate
       else Penultimate

-- | Check if a vowel character has a stress mark (acute or circumflex)
isStressedVowel :: Char -> Bool
isStressedVowel c = c `elem` ("áéíóúâêôû" :: String)

-- | Check if character is a vowel (includes accented vowels and diaeresis variants)
isVowelChar :: Char -> Bool
isVowelChar c = c `elem` ("aäeëiïöoüuáéíóúàèìòùîâêôûǎěǐǒǔ" :: String)

-- | Parse Slot IX based on stress: Case (penultimate) or Vk (ultimate)
parseSlotIXSimple :: Stress -> Maybe Text -> (Maybe Case, Maybe (Illocution, Validation))
parseSlotIXSimple Ultimate (Just v) = (Nothing, parseVk v)
parseSlotIXSimple Monosyllabic (Just v) = (parseCase v, Nothing)
parseSlotIXSimple _ (Just v) = (parseCase v, Nothing)
parseSlotIXSimple _ Nothing = (Nothing, Nothing)

-- | Parse Slot IX for concatenated formatives (always case, never Illocution+Validation)
-- Ultimate/Monosyllabic stress: glottalize the vowel first (shifts to Relational/ST case series)
-- Penultimate: parse as-is
parseConcatSlotIX :: Stress -> Maybe Text -> Maybe Case
parseConcatSlotIX _ Nothing = Nothing
parseConcatSlotIX stress (Just v) =
  let normalized = normalizeAccents v
      vowel = case stress of
        Ultimate -> glottalizeVowel normalized
        Monosyllabic -> glottalizeVowel normalized
        _ -> normalized
  in parseCase vowel

-- | Insert glottal stop into a vowel form (Kotlin: glottalizeVowel)
-- "a" -> "a'a", "öe" -> "ö'e"
glottalizeVowel :: Text -> Text
glottalizeVowel v
  | T.length v == 1 = v <> "'" <> v
  | T.length v == 2 = T.singleton (T.head v) <> "'" <> T.singleton (T.last v)
  | otherwise = v

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

-- | Strip stress marks from vowels for parsing
-- Acute accents map to plain vowels: á→a, é→e, etc.
-- Circumflex accents map to umlauted vowels: â→ä, ê→ë, ô→ö, û→ü
normalizeAccents :: Text -> Text
normalizeAccents = T.map stripAccent
  where
    stripAccent 'á' = 'a'
    stripAccent 'é' = 'e'
    stripAccent 'í' = 'i'
    stripAccent 'ó' = 'o'
    stripAccent 'ú' = 'u'
    stripAccent 'â' = 'ä'
    stripAccent 'ê' = 'ë'
    stripAccent 'ô' = 'ö'
    stripAccent 'û' = 'ü'
    -- Diaeresis variants (hiatus markers)
    stripAccent 'ï' = 'i'
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
  -- Associative (Series 3) - must match vowelFormTable
  , (Associative APL, "ia"), (Associative PUR, "ie")
  , (Associative TRA, "io"), (Associative DFR, "iö")
  , (Associative CRS, "eë"), (Associative TSP, "uö")
  , (Associative CMM, "uo"), (Associative CMP, "ue")
  , (Associative CSD, "ua")
  -- Series 3 alternate forms
  , (Associative APL, "uä"), (Associative PUR, "uë")
  , (Associative TRA, "üä"), (Associative DFR, "üë")
  , (Associative TSP, "öë"), (Associative CMM, "öä")
  , (Associative CMP, "ië"), (Associative CSD, "iä")
  -- Adverbial (Series 4) - must match vowelFormTable
  , (Adverbial FUN, "ao"), (Adverbial TFM, "aö")
  , (Adverbial CLA, "eo"), (Adverbial RSL, "eö")
  , (Adverbial CSM, "oë"), (Adverbial CON, "öe")
  , (Adverbial AVR, "oe"), (Adverbial CVS, "öa")
  , (Adverbial SIT, "oa")
  -- Relational (Series 1 vowels + glottal stop) - must match vowelFormTable
  , (Relational PRN, "a'a"), (Relational DSP, "ä'ä")
  , (Relational COR, "e'e"), (Relational CPS, "i'i")
  , (Relational COM, "ë'i"), (Relational UTL, "ö'ö")
  , (Relational PRD, "o'o"), (Relational RLT, "u'u")
  -- Affinitive (Series 2 vowels + glottal stop) - must match vowelFormTable
  , (Affinitive ACT, "a'i"), (Affinitive ASI, "a'u")
  , (Affinitive ESS, "e'i"), (Affinitive TRM, "e'u")
  , (Affinitive SEL, "ë'u"), (Affinitive CFM, "o'u")
  , (Affinitive DEP, "o'i"), (Affinitive VOC, "u'i")
  -- Spatio-Temporal I (Series 3 vowels + glottal stop) - must match vowelFormTable
  , (SpatioTemporal1 LOC, "i'a"), (SpatioTemporal1 ATD, "i'e")
  , (SpatioTemporal1 ALL, "i'o"), (SpatioTemporal1 ABL, "i'ö")
  , (SpatioTemporal1 ORI, "e'ë"), (SpatioTemporal1 IRL, "u'ö")
  , (SpatioTemporal1 INV, "u'o"), (SpatioTemporal1 NAV, "u'a")
  -- Series 3 alternates with glottal
  , (SpatioTemporal1 LOC, "u'ä"), (SpatioTemporal1 ATD, "u'ë")
  , (SpatioTemporal1 ALL, "ü'ä"), (SpatioTemporal1 ABL, "ü'ë")
  , (SpatioTemporal1 IRL, "ö'ë"), (SpatioTemporal1 INV, "ö'ä")
  , (SpatioTemporal1 NAV, "i'ë")
  -- Spatio-Temporal II (Series 4 vowels + glottal stop) - must match vowelFormTable
  , (SpatioTemporal2 CNR, "a'o"), (SpatioTemporal2 ASS, "a'ö")
  , (SpatioTemporal2 PER, "e'o"), (SpatioTemporal2 PRO, "e'ö")
  , (SpatioTemporal2 PCV, "o'ë"), (SpatioTemporal2 PCR, "ö'e")
  , (SpatioTemporal2 ELP, "o'e"), (SpatioTemporal2 PLM, "ö'a")
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

-- | Ca allomorph desubstitutions (reverse phonotactic substitutions)
-- Applied in order before compositional parsing to recover the canonical form
-- Based on the Kotlin glosser's CA_DESUBSTITUTIONS
-- | Reverse allomorphic substitutions to recover raw Ca form
desubstituteCa :: Text -> Text
desubstituteCa = T.replace "mp" "pp"  -- reverse of pp → mp

-- | Parse Ca consonant cluster
-- Uses common lookup table for frequent forms, desubstitution for allomorphic
-- variants, and falls back to exhaustive reverse map (all 3840 Ca forms).
-- When a bare Ca consonant is missing the M_/NRM "l" suffix (common in
-- formatives where Ca is followed directly by Vc), tries appending "l".
parseCa :: Text -> Maybe ParsedCa
parseCa ca = lookup ca caLookupTable
         <|> fromSlot ca
         <|> tryDesub
         <|> tryGrammarTable ca
  where
    fromSlot t = case parseCaSlot t of
      Just (cfg, aff, persp, ext, ess) ->
        Just (ParsedCa cfg aff persp ext ess)
      Nothing -> Nothing
    tryDesub = let desub = desubstituteCa ca
               in if desub /= ca
                  then lookup desub caLookupTable <|> fromSlot desub
                  else Nothing

-- | Parse Ca using the grammar-table consonant system (ch03)
-- Structure: Affiliation prefix + Configuration + Perspective suffix
-- Grammar affiliation prefixes: CSL=null, ASO="l", COA="r", VAR="ř"
-- Grammar perspective suffixes: M_=null, G_="r", N_="w", A_="y"
tryGrammarTable :: Text -> Maybe ParsedCa
tryGrammarTable ca = listToMaybe
  [ ParsedCa cfg aff persp DEL NRM
  | (aff, affPfx) <- grammarAffiliationPrefixes
  , affPfx `T.isPrefixOf` ca
  , let rest1 = T.drop (T.length affPfx) ca
  , (cfg, cfgC) <- configurationPatterns
  , cfg /= UNI || not (T.null affPfx)  -- UNI only with explicit affiliation
  , cfgC `T.isPrefixOf` rest1
  , let rest2 = T.drop (T.length cfgC) rest1
  , (persp, perspSfx) <- grammarPerspectiveSuffixes
  , perspSfx == rest2
  ]

grammarAffiliationPrefixes :: [(Affiliation, Text)]
grammarAffiliationPrefixes =
  [ (CSL, "")
  , (ASO, "l")
  , (COA, "r")
  , (VAR, "ř")
  ]

grammarPerspectiveSuffixes :: [(Perspective, Text)]
grammarPerspectiveSuffixes =
  [ (M_, "")
  , (G_, "r")
  , (N_, "w")
  , (A_, "y")
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
  , ("ř",   ParsedCa UNI CSL G_ DEL RPV)
  , ("m",   ParsedCa UNI CSL N_ DEL RPV)
  , ("h",   ParsedCa UNI CSL N_ DEL RPV)  -- alternate
  , ("n",   ParsedCa UNI CSL A_ DEL RPV)
  , ("ç",   ParsedCa UNI CSL A_ DEL RPV)  -- alternate
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
