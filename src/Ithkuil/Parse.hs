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
  , parseCase
  , parseCa
  , splitConjuncts
  , isVowelChar
  , isConsonantCluster
  , defaultCa
  , casePatterns
  , slotIVTable
  , vnTable
  , cnMoodTable
  , cnCaseScopeTable
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import Ithkuil.Grammar
import Ithkuil.Phonology (vowelForm)
import qualified Ithkuil.Allomorph as Ca

--------------------------------------------------------------------------------
-- Slot II: Vv (Stem + Version)
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Slot IV: Vr (Function + Specification + Context)
--------------------------------------------------------------------------------

-- | Parse Vr vowel to Slot IV (Function + Specification + Context)
-- All 32 values across 4 context series
parseSlotIV :: Text -> Maybe SlotIV
parseSlotIV v = listToMaybe
  [ slot | (slot, vr) <- slotIVTable, vr == v ]

-- | Complete Vr table: 4 series x 8 values
-- Series 1 uses forms 1,2,3,5,6,7,8,9 (skips form 4 "ë")
-- Series 2-4 use forms 1,2,3,4,6,7,8,9 (skips form 5)
slotIVTable :: [(SlotIV, Text)]
slotIVTable = concat [series1, series2, series3, series4]
  where
    series1 = -- EXS context, Series 1 (skips form 4)
      [ ((STA, BSC, EXS), vowelForm 1 1)  -- a
      , ((STA, CTE, EXS), vowelForm 1 2)  -- ä
      , ((STA, CSV, EXS), vowelForm 1 3)  -- e
      , ((STA, OBJ, EXS), vowelForm 1 5)  -- i
      , ((DYN, OBJ, EXS), vowelForm 1 6)  -- ö
      , ((DYN, CSV, EXS), vowelForm 1 7)  -- o
      , ((DYN, CTE, EXS), vowelForm 1 8)  -- ü
      , ((DYN, BSC, EXS), vowelForm 1 9)  -- u
      ]
    series2 = -- FNC context, Series 2 (skips form 5)
      [ ((STA, BSC, FNC), vowelForm 2 1)  -- ai
      , ((STA, CTE, FNC), vowelForm 2 2)  -- au
      , ((STA, CSV, FNC), vowelForm 2 3)  -- ei
      , ((STA, OBJ, FNC), vowelForm 2 4)  -- eu
      , ((DYN, OBJ, FNC), vowelForm 2 6)  -- ou
      , ((DYN, CSV, FNC), vowelForm 2 7)  -- oi
      , ((DYN, CTE, FNC), vowelForm 2 8)  -- iu
      , ((DYN, BSC, FNC), vowelForm 2 9)  -- ui
      ]
    series3 = -- RPS context, Series 3 (skips form 5)
      [ ((STA, BSC, RPS), vowelForm 3 1)  -- ia
      , ((STA, CTE, RPS), vowelForm 3 2)  -- iä
      , ((STA, CSV, RPS), vowelForm 3 3)  -- ie
      , ((STA, OBJ, RPS), vowelForm 3 4)  -- ië
      , ((DYN, OBJ, RPS), vowelForm 3 6)  -- uö
      , ((DYN, CSV, RPS), vowelForm 3 7)  -- uo
      , ((DYN, CTE, RPS), vowelForm 3 8)  -- ue
      , ((DYN, BSC, RPS), vowelForm 3 9)  -- ua
      ]
    series4 = -- AMG context, Series 4 (skips form 5)
      [ ((STA, BSC, AMG), vowelForm 4 1)  -- ao
      , ((STA, CTE, AMG), vowelForm 4 2)  -- ae
      , ((STA, CSV, AMG), vowelForm 4 3)  -- ea
      , ((STA, OBJ, AMG), vowelForm 4 4)  -- eo
      , ((DYN, OBJ, AMG), vowelForm 4 6)  -- öe
      , ((DYN, CSV, AMG), vowelForm 4 7)  -- oe
      , ((DYN, CTE, AMG), vowelForm 4 8)  -- öa
      , ((DYN, BSC, AMG), vowelForm 4 9)  -- oa
      ]

--------------------------------------------------------------------------------
-- Slot IX: Case (Vc) - All 68 cases
--------------------------------------------------------------------------------

-- | Case vowel patterns - 8 groups mapped to vowel series
-- Groups 1-4 use standard vowel forms (series 1-4)
-- Groups 5-8 use special vowel combinations
casePatterns :: [(Case, Text)]
casePatterns =
  -- Transrelative (Series 1, forms 1-9)
  [ (Transrelative THM, vowelForm 1 1)  -- a
  , (Transrelative INS, vowelForm 1 2)  -- ä
  , (Transrelative ABS, vowelForm 1 3)  -- e
  , (Transrelative AFF, vowelForm 1 5)  -- i
  , (Transrelative STM, vowelForm 1 4)  -- ë
  , (Transrelative EFF, vowelForm 1 6)  -- ö
  , (Transrelative ERG, vowelForm 1 7)  -- o
  , (Transrelative DAT, vowelForm 1 8)  -- ü
  , (Transrelative IND, vowelForm 1 9)  -- u
  -- Appositive (Series 2, forms 1-9)
  , (Appositive POS, vowelForm 2 1)  -- ai
  , (Appositive PRP, vowelForm 2 2)  -- au
  , (Appositive GEN, vowelForm 2 3)  -- ei
  , (Appositive ATT, vowelForm 2 4)  -- eu
  , (Appositive PDC, vowelForm 2 5)  -- ëi
  , (Appositive ITP, vowelForm 2 6)  -- ou
  , (Appositive OGN, vowelForm 2 7)  -- oi
  , (Appositive IDP, vowelForm 2 8)  -- iu
  , (Appositive PAR, vowelForm 2 9)  -- ui
  -- Associative (Series 3, forms 1-9)
  , (Associative APL, vowelForm 3 1)  -- ia
  , (Associative PUR, vowelForm 3 2)  -- iä
  , (Associative TRA, vowelForm 3 3)  -- ie
  , (Associative DFR, vowelForm 3 4)  -- ië
  , (Associative CRS, vowelForm 3 5)  -- ëu
  , (Associative TSP, vowelForm 3 6)  -- uö
  , (Associative CMM, vowelForm 3 7)  -- uo
  , (Associative CMP, vowelForm 3 8)  -- ue
  , (Associative CSD, vowelForm 3 9)  -- ua
  -- Adverbial (Series 4, forms 1-9)
  , (Adverbial FUN, vowelForm 4 1)  -- ao
  , (Adverbial TFM, vowelForm 4 2)  -- ae
  , (Adverbial CLA, vowelForm 4 3)  -- ea
  , (Adverbial RSL, vowelForm 4 4)  -- eo
  , (Adverbial CSM, vowelForm 4 5)  -- eë
  , (Adverbial CON, vowelForm 4 6)  -- öe
  , (Adverbial AVR, vowelForm 4 7)  -- oe
  , (Adverbial CVS, vowelForm 4 8)  -- öa
  , (Adverbial SIT, vowelForm 4 9)  -- oa
  -- Relational (special vowel pairs)
  , (Relational PRN, "ao")
  , (Relational DSP, "aö")
  , (Relational COR, "eo")
  , (Relational CPS, "eö")
  , (Relational COM, "oë")
  , (Relational UTL, "öe")
  , (Relational PRD, "oe")
  , (Relational RLT, "öa")
  -- Affinitive
  , (Affinitive ACT, "oa")
  , (Affinitive ASI, "öä")
  , (Affinitive ESS, "ea")
  , (Affinitive TRM, "eä")
  , (Affinitive SEL, "ëë")
  , (Affinitive CFM, "öö")
  , (Affinitive DEP, "ëö")
  , (Affinitive VOC, "öë")
  -- Spatio-Temporal I (glottal stop vowel pairs)
  , (SpatioTemporal1 LOC, "a'a")
  , (SpatioTemporal1 ATD, "ä'ä")
  , (SpatioTemporal1 ALL, "e'e")
  , (SpatioTemporal1 ABL, "ë'ë")
  , (SpatioTemporal1 ORI, "ë'a")
  , (SpatioTemporal1 IRL, "ö'ö")
  , (SpatioTemporal1 INV, "o'o")
  , (SpatioTemporal1 NAV, "ü'ü")
  -- Spatio-Temporal II (glottal stop vowel pairs)
  , (SpatioTemporal2 CNR, "i'i")
  , (SpatioTemporal2 ASS, "u'u")
  , (SpatioTemporal2 PER, "a'i")
  , (SpatioTemporal2 PRO, "i'a")
  , (SpatioTemporal2 PCV, "e'i")
  , (SpatioTemporal2 PCR, "i'e")
  , (SpatioTemporal2 ELP, "u'a")
  , (SpatioTemporal2 PLM, "a'u")
  ]

-- | Parse case from Vc vowel
parseCase :: Text -> Maybe Case
parseCase vc = listToMaybe
  [ c | (c, pat) <- casePatterns, pat == vc ]

--------------------------------------------------------------------------------
-- Slot VI: Ca Complex
--------------------------------------------------------------------------------

-- | Parsed Ca complex
data ParsedCa = ParsedCa
  { pcConfig      :: Configuration
  , pcAffiliation :: Affiliation
  , pcPerspective :: Perspective
  , pcExtension   :: Extension
  , pcEssence     :: Essence
  } deriving (Show, Eq)

-- | Default Ca values (UNI/CSL/M_/DEL/NRM = "l")
defaultCa :: ParsedCa
defaultCa = ParsedCa UNI CSL M_ DEL NRM

-- | Parse Ca consonant cluster using pre-generated reverse lookup
parseCa :: Text -> Maybe ParsedCa
parseCa ca = case Ca.parseCaSlot ca of
  Just (co, af, pe, ex, es) -> Just $ ParsedCa co af pe ex es
  Nothing -> Nothing

--------------------------------------------------------------------------------
-- Slot VIII: Vn (Valence) and Cn (Mood/Case-Scope)
--------------------------------------------------------------------------------

-- | Vn vowel forms (Series 1 = Valence)
vnTable :: [(Valence, Text)]
vnTable =
  [ (MNO, vowelForm 1 1)  -- a
  , (PRL, vowelForm 1 2)  -- ä
  , (CRO, vowelForm 1 3)  -- e
  , (RCP, vowelForm 1 5)  -- i
  , (CPL, vowelForm 1 4)  -- ë
  , (DUP, vowelForm 1 6)  -- ö
  , (DEM, vowelForm 1 7)  -- o
  , (CNG, vowelForm 1 8)  -- ü
  , (PTI, vowelForm 1 9)  -- u
  ]

-- | Cn consonant forms for Mood
cnMoodTable :: [(Mood, Text)]
cnMoodTable =
  [ (FAC, "h")
  , (SUB, "hl")
  , (ASM, "hr")
  , (SPC, "hw")
  , (COU, "hm")
  , (HYP, "hn")
  ]

-- | Cn consonant forms for Case-Scope
cnCaseScopeTable :: [(CaseScope, Text)]
cnCaseScopeTable =
  [ (CCN, "w")
  , (CCA, "y")
  , (CCS, "h")
  , (CCQ, "hw")
  , (CCP, "hl")
  , (CCV, "hr")
  ]

-- | Parse Vn vowel to Valence
parseVn :: Text -> Maybe Valence
parseVn v = listToMaybe [val | (val, f) <- vnTable, f == v]

-- | Parse Cn consonant to Mood
parseCnMood :: Text -> Maybe Mood
parseCnMood c = listToMaybe [m | (m, f) <- cnMoodTable, f == c]

-- | Parse Cn consonant to Case-Scope
parseCnCaseScope :: Text -> Maybe CaseScope
parseCnCaseScope c = listToMaybe [cs | (cs, f) <- cnCaseScopeTable, f == c]

--------------------------------------------------------------------------------
-- Parsed Formative
--------------------------------------------------------------------------------

-- | Parsed formative with all identified slots
data ParsedFormative = ParsedFormative
  { pfSlotII  :: SlotII          -- Stem + Version
  , pfRoot    :: Root            -- Cr (root consonants)
  , pfSlotIV  :: SlotIV          -- Function + Specification + Context
  , pfCa      :: [Text]          -- Ca complex (raw conjuncts)
  , pfCaParsed :: Maybe ParsedCa -- Parsed Ca values
  , pfCase    :: Maybe Case      -- Vc (case vowel)
  , pfConjuncts :: [Text]        -- Original conjunct split
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Formative Parsing
--------------------------------------------------------------------------------

-- | Parse a formative, handling both vowel-initial and consonant-initial words
parseFormativeReal :: Text -> Maybe ParsedFormative
parseFormativeReal word = do
  let parts = splitConjuncts word
  case parts of
    [] -> Nothing
    (first:_) ->
      if isConsonantCluster first
        then parseConsonantInitial parts
        else parseVowelInitial parts

-- | Check if text is a consonant cluster (starts with consonant)
isConsonantCluster :: Text -> Bool
isConsonantCluster t = case T.uncons t of
  Nothing -> False
  Just (c, _) -> not (isVowelChar c)

-- | Parse consonant-initial word (elided Vv = default S1/PRC)
parseConsonantInitial :: [Text] -> Maybe ParsedFormative
parseConsonantInitial parts = case parts of
  (cr:vr:rest) -> do
    slotIV <- parseSlotIV vr
    let (caRest, vcRest) = splitCaVc rest
        caseM = parseCase =<< listToMaybe vcRest
        caConsonants = filter (not . T.null) $ filter isConsonantCluster caRest
        caParsedM = parseCa =<< listToMaybe caConsonants
    Just ParsedFormative
      { pfSlotII = (S1, PRC)
      , pfRoot = Root cr
      , pfSlotIV = slotIV
      , pfCa = caRest
      , pfCaParsed = caParsedM
      , pfCase = caseM
      , pfConjuncts = parts
      }
  _ -> Nothing

-- | Parse vowel-initial word (explicit Vv)
parseVowelInitial :: [Text] -> Maybe ParsedFormative
parseVowelInitial parts = case parts of
  (vv:cr:vr:rest) -> do
    slotII <- parseSlotII vv
    slotIV <- parseSlotIV vr
    let (caRest, vcRest) = splitCaVc rest
        caseM = parseCase =<< listToMaybe vcRest
        caConsonants = filter (not . T.null) $ filter isConsonantCluster caRest
        caParsedM = parseCa =<< listToMaybe caConsonants
    Just ParsedFormative
      { pfSlotII = slotII
      , pfRoot = Root cr
      , pfSlotIV = slotIV
      , pfCa = caRest
      , pfCaParsed = caParsedM
      , pfCase = caseM
      , pfConjuncts = parts
      }
  _ -> Nothing

-- | Split remaining conjuncts into Ca complex and Vc
splitCaVc :: [Text] -> ([Text], [Text])
splitCaVc parts =
  let isVowelCluster t = not (T.null t) && isVowelChar (T.head t)
      revParts = reverse parts
      (_trailingC, afterC) = span (not . isVowelCluster) revParts
      (vcRev, caRev) = span isVowelCluster afterC
  in (reverse caRev, reverse vcRev)

-- | Parse a simple word structure: Vv-Cr-Vr-Ca
parseSimpleWord :: Text -> Maybe (SlotII, Root, SlotIV)
parseSimpleWord word = do
  pf <- parseFormativeReal word
  return (pfSlotII pf, pfRoot pf, pfSlotIV pf)

--------------------------------------------------------------------------------
-- Phonological Helpers
--------------------------------------------------------------------------------

-- | Check if character is a vowel (includes accented vowels)
isVowelChar :: Char -> Bool
isVowelChar c = c `elem` ("aäeëiöoüuáéíóúàèìòùîâêôûǎěǐǒǔ" :: String)

-- | Split text into consonant/vowel conjuncts
splitConjuncts :: Text -> [Text]
splitConjuncts = filter (not . T.null) . T.groupBy sameType
  where
    sameType a b = isVowelChar a == isVowelChar b
