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
  , defaultCa
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import Ithkuil.Phonology
import Ithkuil.Grammar

-- | Parse Vv vowel to Slot II (Stem + Version)
parseSlotII :: Text -> Maybe SlotII
parseSlotII v = listToMaybe
  [ slot | (slot, vv) <- slotIITable, vv == v ]
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
  [ slot | (slot, vr) <- slotIVTable, vr == v ]
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
  , pfCase    :: Maybe Case      -- Vc (case vowel)
  , pfStress  :: Stress          -- Detected stress pattern
  , pfConjuncts :: [Text]        -- Original conjunct split
  } deriving (Show, Eq)

-- | Parse a formative, handling both vowel-initial and consonant-initial words
-- Consonant-initial words have elided Vv (defaults to S1/PRC = "a")
parseFormativeReal :: Text -> Maybe ParsedFormative
parseFormativeReal word = do
  let parts = splitConjuncts word
      stress = detectStressSimple word
  case parts of
    [] -> Nothing
    (first:_) ->
      if isConsonantCluster first
        then parseConsonantInitial stress parts  -- Elided Vv
        else parseVowelInitial stress parts      -- Normal Vv-Cr-Vr-Ca...

-- | Check if text is a consonant cluster (starts with consonant)
isConsonantCluster :: Text -> Bool
isConsonantCluster t = case T.uncons t of
  Nothing -> False
  Just (c, _) -> not (isVowelChar c)

-- | Parse consonant-initial word (elided Vv = default S1/PRC)
parseConsonantInitial :: Stress -> [Text] -> Maybe ParsedFormative
parseConsonantInitial stress parts = case parts of
  -- Minimal: Cr-Vr-Ca (e.g., "mal")
  (cr:vr:rest) -> do
    slotIV <- parseSlotIV vr
    let (caRest, vcRest) = splitCaVc rest
        caseM = parseCase =<< listToMaybe vcRest
        -- Try to parse Ca from consonants in caRest
        caConsonants = filter (not . T.null) $ filter isConsonantCluster caRest
        caParsedM = parseCa =<< listToMaybe caConsonants
    Just ParsedFormative
      { pfSlotII = (S1, PRC)  -- Default elided value
      , pfRoot = Root cr
      , pfSlotIV = slotIV
      , pfCa = caRest
      , pfCaParsed = caParsedM
      , pfCase = caseM
      , pfStress = stress
      , pfConjuncts = parts
      }
  _ -> Nothing

-- | Parse vowel-initial word (explicit Vv)
parseVowelInitial :: Stress -> [Text] -> Maybe ParsedFormative
parseVowelInitial stress parts = case parts of
  (vv:cr:vr:rest) -> do
    slotII <- parseSlotII vv
    slotIV <- parseSlotIV vr
    let (caRest, vcRest) = splitCaVc rest
        caseM = parseCase =<< listToMaybe vcRest
        -- Try to parse Ca from consonants in caRest
        caConsonants = filter (not . T.null) $ filter isConsonantCluster caRest
        caParsedM = parseCa =<< listToMaybe caConsonants
    Just ParsedFormative
      { pfSlotII = slotII
      , pfRoot = Root cr
      , pfSlotIV = slotIV
      , pfCa = caRest
      , pfCaParsed = caParsedM
      , pfCase = caseM
      , pfStress = stress
      , pfConjuncts = parts
      }
  _ -> Nothing

-- | Split remaining conjuncts into Ca complex and Vc
-- The last vowel cluster is typically Vc (case), rest is Ca
-- May have a final consonant after the case vowel (Slot X)
splitCaVc :: [Text] -> ([Text], [Text])
splitCaVc parts =
  let isVowelCluster t = not (T.null t) && isVowelChar (T.head t)
      revParts = reverse parts
      -- Skip trailing consonants (Slot X), then find the case vowel
      (trailingC, afterC) = span (not . isVowelCluster) revParts
      (vcRev, caRev) = span isVowelCluster afterC
      -- Ca = everything before Vc (plus trailing consonants go back to Ca? No, they're Slot X)
  in (reverse caRev, reverse vcRev)

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
  -- Relational (Series 5)
  , (Relational PRN, "ao"), (Relational DSP, "aö")
  , (Relational COR, "eo"), (Relational CPS, "eö")
  , (Relational COM, "oë"), (Relational UTL, "öe")
  , (Relational PRD, "oe"), (Relational RLT, "öa")
  -- Affinitive (Series 6)
  , (Affinitive ACT, "oa"), (Affinitive ASI, "öä")
  , (Affinitive ESS, "ea"), (Affinitive TRM, "eä")
  , (Affinitive SEL, "ëë"), (Affinitive CFM, "öö")
  , (Affinitive DEP, "ëö"), (Affinitive VOC, "öë")
  -- Spatio-Temporal I (Series 7)
  , (SpatioTemporal1 LOC, "a'a"), (SpatioTemporal1 ATD, "ä'ä")
  , (SpatioTemporal1 ALL, "e'e"), (SpatioTemporal1 ABL, "ë'ë")
  , (SpatioTemporal1 ORI, "ë'a"), (SpatioTemporal1 IRL, "ö'ö")
  , (SpatioTemporal1 INV, "o'o"), (SpatioTemporal1 NAV, "ü'ü")
  -- Spatio-Temporal II (Series 8)
  , (SpatioTemporal2 CNR, "i'i"), (SpatioTemporal2 ASS, "u'u")
  , (SpatioTemporal2 PER, "a'i"), (SpatioTemporal2 PRO, "i'a")
  , (SpatioTemporal2 PCV, "e'i"), (SpatioTemporal2 PCR, "i'e")
  , (SpatioTemporal2 ELP, "u'a"), (SpatioTemporal2 PLM, "a'u")
  ]

-- | Parse case from Vc vowel
parseCase :: Text -> Maybe Case
parseCase vc = listToMaybe
  [ c | (c, pattern) <- casePatterns, pattern == vc ]

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
  , ("sl",  ParsedCa UNI CSL M_ DEL NRM)  -- DPX
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
