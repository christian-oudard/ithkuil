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
  , parseAffixVr
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (listToMaybe, isJust)
import Ithkuil.Phonology
import Ithkuil.Grammar

-- | Parse Vv vowel to Slot II (Stem + Version)
-- The Vv table is a direct mapping of 8 specific vowels:
--   S1/PRC=a, S1/CPT=ä, S2/PRC=e, S2/CPT=i, S3/PRC=u, S3/CPT=ü, S0/PRC=o, S0/CPT=ö
-- Series 2-4 diphthongs (ai, ia, ao, etc.) are also valid Vv,
-- carrying the same stem/version as their Series 1 base + additional implied semantics.
parseSlotII :: Text -> Maybe SlotII
parseSlotII v = listToMaybe
  [ slot | (slot, vv) <- slotIITable, vv == normalizeAccents v ]
  where
    -- Series 1: base 8 vowels
    baseMappings =
      [ ((S1, PRC), "a"),  ((S1, CPT), "ä")
      , ((S2, PRC), "e"),  ((S2, CPT), "i")
      , ((S3, PRC), "u"),  ((S3, CPT), "ü")
      , ((S0, PRC), "o"),  ((S0, CPT), "ö")
      ]
    -- Series 2-4: diphthong Vv forms carry same stem/version as corresponding form
    -- Form→Stem: 1,2→S1; 3,4→S2; 9,8→S3; 7,6→S0
    -- Form→Version: odd→PRC, even→CPT
    seriesMappings = concatMap seriesEntries [2..4]
    seriesEntries s =
      [ ((S1, PRC), vowelForm s 1), ((S1, CPT), vowelForm s 2)
      , ((S2, PRC), vowelForm s 3), ((S2, CPT), vowelForm s 4)
      , ((S3, PRC), vowelForm s 9), ((S3, CPT), vowelForm s 8)
      , ((S0, PRC), vowelForm s 7), ((S0, CPT), vowelForm s 6)
      ]
    slotIITable = baseMappings ++ seriesMappings

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
  , ((A_, NRM), "j",  "y")
  -- RPV forms (standalone, after-consonant)
  , ((M_, RPV), "tļ", "l")
  , ((G_, RPV), "ř",  "ř")
  , ((N_, RPV), "m",  "m")
  , ((N_, RPV), "h",  "h")    -- alternate
  , ((A_, RPV), "n",  "n")
  , ((A_, RPV), "ç",  "ç")    -- alternate
  ]

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
  , pfStress  :: Stress          -- Detected stress pattern
  , pfConjuncts :: [Text]        -- Original conjunct split
  , pfCsRootDegree :: Maybe Int  -- Cs-root degree (Nothing for normal roots)
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
vvSeries vv = case normalizeAccents vv of
  "a"  -> 1; "ä" -> 1
  "e"  -> 1; "i" -> 1
  "u"  -> 1; "ü" -> 1
  "o"  -> 1; "ö" -> 1
  "ai" -> 2; "au" -> 2; "ei" -> 2; "eu" -> 2
  "ëu" -> 2; "ou" -> 2; "oi" -> 2; "iu" -> 2; "ui" -> 2
  "ia" -> 3; "iä" -> 3; "ie" -> 3; "ië" -> 3
  "ëo" -> 3; "iö" -> 3; "io" -> 3; "iü" -> 3
  "ua" -> 4; "uä" -> 4; "ue" -> 4; "uë" -> 4
  "ëa" -> 4; "uö" -> 4; "uo" -> 4; "uü" -> 4
  _ -> 1

-- | Check if a Vv value is a special Cs-root marker
-- These signal that the "root" consonant is actually a Cs affix form
isSpecialVv :: Text -> Bool
isSpecialVv v = normalizeAccents v `elem` ["ëi", "eë", "ëu", "oë"]

-- | Parse special Vv for Cs-root formatives
-- Returns (Version, Maybe Function) - function is Nothing for reference roots (ae, ea)
parseSpecialVv :: Text -> Maybe (Version, Function)
parseSpecialVv v = case normalizeAccents v of
  "ëi" -> Just (PRC, STA)
  "eë" -> Just (PRC, DYN)
  "ëu" -> Just (CPT, STA)
  "oë" -> Just (CPT, DYN)
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
      -- Use vowel form table to get (series, form)
      (series, form) <- lookupSeriesForm nv
      let ctx = case series of
            1 -> EXS; 2 -> FNC; 3 -> RPS; 4 -> AMG; _ -> EXS
      Just (form, ctx)
  where
    lookupSeriesForm v = listToMaybe
      [ (s, f) | s <- [1..4], f <- [1..9], vowelForm s f == v ]

-- | Parse a formative, handling both vowel-initial and consonant-initial words
-- Consonant-initial words have elided Vv (defaults to S1/PRC = "a")
-- Words starting with w/y + vowel are treated as vowel-initial (w/y is a Cc shortcut)
-- Words starting with h/hl/hm/hw/hr/hn are concatenated formatives
parseFormativeReal :: Text -> Maybe ParsedFormative
parseFormativeReal word = do
  let lword = T.toLower word
      -- Strip ç sentence prefix (çë, ça, çw, çy, çç patterns)
      -- çë uses "ë" as default Vv marker — strip both ç and ë
      stripped = case T.uncons lword of
        Just ('ç', rest) -> case T.uncons rest of
          Just ('ë', rest2) | not (T.null rest2) -> rest2  -- çë = strip prefix + default Vv
          Just ('ç', _) -> rest  -- çç = strip one ç, keep the other (PRX shortcut)
          _ | not (T.null rest) -> rest
          _ -> lword
        _ -> lword
      parts = splitConjuncts stripped
      stress = detectStressSimple stripped
  case parts of
    [] -> Nothing
    -- Check for Cc consonant (concatenation/shortcut): w, y, h, hl, hm, hw, hr, hn
    (cc:rest) | not (null rest) ->
      let (concatM, scM) = parseCc cc
      in case (concatM, scM) of
        -- Pure shortcut (w or y): parse with shortcut Ca
        (Nothing, Just sc) -> do
          pf <- parseVowelInitialWithShortcut sc stress rest
          return pf
        -- Concatenation with shortcut (hl, hm, hr, hn): parse with shortcut Ca + concat
        (Just ct, Just sc) -> do
          pf <- parseVowelInitialWithShortcut sc stress rest
          return pf { pfConcatenation = Just ct }
        -- Concatenation without shortcut (h, hw): parse rest normally
        (Just ct, Nothing) -> do
          pf <- parseRestAsFormative stress rest
          return pf { pfConcatenation = Just ct }
        -- No Cc match: normal parsing
        (Nothing, Nothing) ->
          if isConsonantCluster cc
            then parseConsonantInitial stress parts  -- Elided Vv
            else parseVowelInitial stress parts      -- Normal Vv-Cr-Vr-Ca...
    (first:_) ->
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
parseVowelInitialWithShortcut :: CcShortcut -> Stress -> [Text] -> Maybe ParsedFormative
parseVowelInitialWithShortcut sc stress parts = case parts of
  (vv:cr:rest) -> do
    slotII <- parseSlotII vv
    let series = vvSeries vv
        scCa = shortcutCa sc series
        -- Split rest into VxCs affix pairs and final Vc/Vk
        -- Merge glottal-vowel patterns first, then take last vowel as Vc/Vk
        merged = mergeGlottalVowels rest
        (affixParts, lastVowel) = splitAfterShortcut merged
        (caseM, illocValM) = parseSlotIXSimple stress lastVowel
        -- Prepend empty placeholder for Ca (extractAffixes skips first element)
        caWithAffixes = if null affixParts then [] else "" : affixParts
    Just ParsedFormative
      { pfConcatenation = Nothing
      , pfSlotII = slotII
      , pfRoot = Root cr
      , pfSlotIV = (STA, BSC, EXS)  -- Shortcuts elide Vr to default
      , pfCa = caWithAffixes        -- Store VxCs pairs for affix extraction
      , pfCaParsed = Just scCa
      , pfSlotVIII = Nothing
      , pfCase = caseM
      , pfIllocVal = illocValM
      , pfStress = stress
      , pfConjuncts = parts
      , pfCsRootDegree = Nothing
      }
  _ -> Nothing

-- | Split conjuncts after shortcut Cr into (affix parts, last vowel)
-- Takes [V, C, V, C, ..., V] and returns (VxCs affix pairs as [V,C,...], last V)
splitAfterShortcut :: [Text] -> ([Text], Maybe Text)
splitAfterShortcut [] = ([], Nothing)
splitAfterShortcut parts =
  let isVowelCluster t = not (T.null t) && isVowelChar (T.head t)
      revParts = reverse parts
      -- Find the last vowel cluster (Vc/Vk)
      (trailingC, afterC) = span (not . isVowelCluster) revParts
  in case afterC of
    (vc:rest) ->
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
          , pfStress = stress
          , pfConjuncts = parts
          , pfCsRootDegree = Nothing
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
              { pfConcatenation = Nothing
              , pfSlotII = (S1, PRC)
              , pfRoot = Root cr
              , pfSlotIV = slotIV
              , pfCa = caRest
              , pfCaParsed = caParsedM
              , pfSlotVIII = Nothing
              , pfCase = caseM
              , pfIllocVal = illocValM
              , pfStress = stress
              , pfConjuncts = parts
              , pfCsRootDegree = Nothing
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
              , pfStress = stress
              , pfConjuncts = parts
              , pfCsRootDegree = Nothing
              }
  _ -> Nothing

-- | Parse vowel-initial word (explicit Vv)
-- First checks for special Cs-root Vv values, then tries normal parsing
parseVowelInitial :: Stress -> [Text] -> Maybe ParsedFormative
parseVowelInitial stress parts = case parts of
  (vv:cr:vr:rest)
    | isSpecialVv vv -> parseCsRootFormative stress vv cr vr rest
    | otherwise -> do
    slotII <- parseSlotII vv
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
          , pfStress = stress
          , pfConjuncts = parts
          , pfCsRootDegree = Nothing
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
              { pfConcatenation = Nothing
              , pfSlotII = slotII
              , pfRoot = Root cr
              , pfSlotIV = slotIV
              , pfCa = caRest
              , pfCaParsed = caParsedM
              , pfSlotVIII = Nothing
              , pfCase = caseM
              , pfIllocVal = illocValM
              , pfStress = stress
              , pfConjuncts = parts
              , pfCsRootDegree = Nothing
              }
          -- If Vr parse fails, vr might actually be Vc/Vk with elided Vr
          Nothing -> tryElidedVr slotII cr vr stress parts
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
      , pfStress = stress
      , pfConjuncts = parts
      , pfCsRootDegree = Nothing
      }
  _ -> Nothing

-- | Parse a Cs-root formative (special Vv: ëi, eë, ëu, oë)
-- The "root" consonant is a Cs affix form; Vr encodes degree + context
parseCsRootFormative :: Stress -> Text -> Text -> Text -> [Text] -> Maybe ParsedFormative
parseCsRootFormative stress vv cs vr rest = do
  (version, func) <- parseSpecialVv vv
  (degree, ctx) <- parseAffixVr vr
  let (caRest, vcRest) = splitCaVc rest
      lastVowel = listToMaybe vcRest
      (caseM, illocValM) = parseSlotIXSimple stress lastVowel
      caConsonants = filter (not . T.null) $ filter isConsonantCluster caRest
      caParsedM = parseCa =<< listToMaybe caConsonants
  Just ParsedFormative
    { pfConcatenation = Nothing
    , pfSlotII = (S1, version)  -- Stem not meaningful for Cs-root
    , pfRoot = Root cs           -- Cs affix consonant stored as root
    , pfSlotIV = (func, BSC, ctx) -- Function from Vv, context from Vr
    , pfCa = caRest
    , pfCaParsed = caParsedM
    , pfSlotVIII = Nothing
    , pfCase = caseM
    , pfIllocVal = illocValM
    , pfStress = stress
    , pfConjuncts = vv : cs : vr : rest
    , pfCsRootDegree = Just degree
    }

-- | Try parsing with elided Vr: the consonant cluster contains Cr+Ca merged
-- and the next vowel is Vc/Vk instead of Vr
tryElidedVr :: SlotII -> Text -> Text -> Stress -> [Text] -> Maybe ParsedFormative
tryElidedVr slotII crca vcvk stress parts = do
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
    , pfStress = stress
    , pfConjuncts = parts
    , pfCsRootDegree = Nothing
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
detectStressSimple word
  | T.any isStressedVowel word =
    let conjs = splitConjuncts word
        syllables = filter (\t -> not (T.null t) && isVowelChar (T.head t)) conjs
        syllCount = length syllables
        hasStress t = T.any isStressedVowel t
        stressPos = length (takeWhile (not . hasStress) syllables) + 1
    in if stressPos == syllCount then Ultimate
       else if stressPos <= syllCount - 2 then Antepenultimate
       else Penultimate
  | otherwise = Penultimate

-- | Check if a vowel character has a stress mark (acute or circumflex)
isStressedVowel :: Char -> Bool
isStressedVowel c = c `elem` ("áéíóúâêôû" :: String)

-- | Check if character is a vowel (includes accented vowels and diaeresis variants)
isVowelChar :: Char -> Bool
isVowelChar c = c `elem` ("aäeëiïöoüuáéíóúàèìòùîâêôûǎěǐǒǔ" :: String)

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

-- | Ca allomorph desubstitutions (reverse phonotactic substitutions)
-- Applied in order before compositional parsing to recover the canonical form
-- Based on the Kotlin glosser's CA_DESUBSTITUTIONS
desubstituteCa :: Text -> Text
desubstituteCa ca = foldl (\t f -> f t) ca desubSteps
  where
    unvoiced = "stckpţfçšč" :: String
    isUnvoiced c = c `elem` unvoiced
    desubSteps =
      [ T.replace "ḑy" "ţţ"
      , T.replace "vw" "ff"
      -- Context-sensitive: ţ/ḑ after context → bn; f/v after context → bm
      , replaceAfterVoiced isUnvoiced 'ţ' 'ḑ' "bn"
      , replaceAfterVoiced isUnvoiced 'f' 'v' "bm"
      -- x/ň → gm/gn (when not at start)
      , replaceNonInitial "xw" "çx"
      , T.replace "ňn" "ngn"
      , replaceNonInitial "ň" "gn"
      , replaceNonInitial "x" "gm"
      , T.replace "ňš" "řř", T.replace "ňs" "řr"
      , T.replace "nš" "rř", T.replace "ns" "rr"
      , T.replace "nd" "çy", T.replace "ng" "kg", T.replace "mb" "pb"
      , T.replace "pļ" "ll", T.replace "nk" "kk", T.replace "nt" "tt", T.replace "mp" "pp"
      ]
    -- Replace 'unv' after unvoiced char or 'voiced' after voiced char with 'to'
    replaceAfterVoiced isUnv unv voiced to t =
      let chars = T.unpack t
          go [] = []
          go [c] = [c]
          go (prev:c:rest)
            | c == unv && isUnv prev = prev : T.unpack to ++ go rest
            | c == voiced && not (isUnv prev) = prev : T.unpack to ++ go rest
            | otherwise = prev : go (c:rest)
      in T.pack (go chars)
    -- Replace pattern when not at the start of string
    replaceNonInitial pat repl t = case T.uncons t of
      Nothing -> t
      Just (first, rest) -> T.singleton first <> T.replace pat repl rest

-- | Parse Ca consonant cluster
-- Uses compositional decomposition: Configuration + Extension + Affiliation + Perspective/Essence
-- Falls back to common lookup table for standard forms, then tries desubstitution
parseCa :: Text -> Maybe ParsedCa
parseCa ca = case lookup ca caLookupTable of
  Just pc -> Just pc
  Nothing -> case tryCompositionalParse ca of
    Just pc -> Just pc
    Nothing -> let desub = desubstituteCa ca
               in if desub /= ca
                  then case lookup desub caLookupTable of
                    Just pc -> Just pc
                    Nothing -> tryCompositionalParse desub
                  else Nothing

-- | Try to parse Ca by decomposing into components left-to-right:
-- Affiliation + Configuration + Extension + Perspective/Essence
tryCompositionalParse :: Text -> Maybe ParsedCa
tryCompositionalParse ca = listToMaybe
  [ ParsedCa cfg aff persp ext ess
  -- Affiliation is leftmost (non-standalone form uses l/r/ř prefix)
  | (aff, affForm, _) <- affiliationPatterns
  , let affF = affForm  -- Non-standalone (before other components)
  , affF `T.isPrefixOf` ca
  , let rest1 = T.drop (T.length affF) ca
  -- Configuration
  , (cfg, cfgForm) <- configurationPatterns
  , cfgForm `T.isPrefixOf` rest1
  , let rest2 = T.drop (T.length cfgForm) rest1
  -- Extension (voiceless after non-UNI, voiced for UNI)
  , (ext, extAfterC, extStandalone) <- extensionPatterns
  , let extF = if cfg == UNI then extStandalone else extAfterC
  , extF `T.isPrefixOf` rest2
  , let rest3 = T.drop (T.length extF) rest2
  -- Perspective + Essence is rightmost
  , ((persp, ess), standalone, afterC) <- perspectiveEssencePatterns
  , let peF = if T.null rest1 && T.null (T.drop (T.length affF) ca)
              then standalone  -- Standalone: only affiliation present
              else afterC      -- After consonant
  , peF == rest3
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
