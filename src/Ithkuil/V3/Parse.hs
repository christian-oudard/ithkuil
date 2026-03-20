{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V3 (Elartkha) Parsing
--
-- Parses V3 formatives from their romanized form into structured Formative values.
-- Contains all vowel/consonant encoding tables (Vr, Vc, CiVi, Vf, Cb).
--
-- Simplified formative structure:
--   [Tone] Vr + Cr + Vc (+Ci+Vi) + Ca (+VxCx) (+Vf (+Cb))
module Ithkuil.V3.Parse
  ( -- * Lookup tables
    vrTable
  , vrTableReverse
  , vcTable
  , vcTableReverse
  , civiTable
  , civiTableReverse
  , vfTable
  , vfTableReverse
  , cbTable
  , cbTableReverse
    -- * Ca table (loaded from file)
  , CaTables(..)
  , loadCaTables
    -- * Parsing
  , parseFormative
  , parseFormativeWithCa
  , ParseError(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Ithkuil.V3.Grammar

--------------------------------------------------------------------------------
-- Vr table: Function × Pattern × Stem → vowel(s)
-- Order: Function (STA,DYN,MNF,DSC) × Pattern (P1,P2,P3) × Stem (S1,S2,S3)
-- Pipe-separated alternatives (e.g. "î|û" means either "î" or "û")
-- 36 entries = 4×3×3
--------------------------------------------------------------------------------

vrEntries :: [(SlotVr, [Text])]
vrEntries = zip keys (map splitAlts rawVr)
  where
    keys = [(f, p, s) | f <- allOf, p <- allOf, s <- allOf]
    rawVr =
      -- STA: P1×S1, P1×S2, P1×S3, P2×S1, P2×S2, P2×S3, P3×S1, P3×S2, P3×S3
      [ "|a",     "e",      "u"
      , "o",      "ö",      "î|û"
      , "â",      "ê",      "ô"
      -- DYN
      , "i",      "ai",     "ei"
      , "au",     "eu",     "iu"
      , "ia|ua",  "ie|ue",  "io|uo"
      -- MNF
      , "ui",     "ü|ou",   "ëi"
      , "ae",     "ea",     "oa"
      , "üa|aì",  "iù|uì", "iö|uö"
      -- DSC
      , "oi",     "eo",     "eö"
      , "oe",     "öe",     "ëu"
      , "üo|oì",  "üe|eì", "üö|aù"
      ]

splitAlts :: Text -> [Text]
splitAlts t = case T.splitOn "|" t of
  ["", x] -> [x]  -- "|a" → ["a"] (leading pipe = the empty string is not a valid form)
  xs      -> filter (not . T.null) xs

vrTable :: Map SlotVr [Text]
vrTable = Map.fromList vrEntries

vrTableReverse :: Map Text SlotVr
vrTableReverse = Map.fromList
  [(v, k) | (k, vs) <- vrEntries, v <- vs]

--------------------------------------------------------------------------------
-- Vc table: Case → vowel(s)
-- 96 entries (72 base + 24 compound)
-- Order matches the cases tuple in grammar_tables.py
--------------------------------------------------------------------------------

allCases :: [Case]
allCases = map Transrelative allOf
        ++ map Possessive allOf
        ++ map Associative allOf
        ++ map Adverbial allOf
        ++ map Relational allOf
        ++ map Temporal1 allOf
        ++ map Temporal2 allOf
        ++ [Compound g s | g <- allOf, s <- allOf]

vcRaw :: [Text]
vcRaw =
  -- Transrelative: OBL IND ABS ERG EFF AFF DAT INS ACT DER
  [ "a",  "u",  "e",  "o",  "ö",  "i",  "ü|a'e",  "ai",  "ei",  "ui"
  -- Possessive: SIT POS PRP GEN ATT PDC ITP OGN PAR CRS
  , "oi",  "â",  "î|û",  "ê",  "ô",  "ëi",  "öi",  "ae",  "ia|ua",  "ie|ue"
  -- Associative: CPS PRD MED APL PUR CSD ESS ASI FUN TFM
  , "io|uo",  "iö|uö",  "a'",  "u'",  "e'",  "o'",  "ea",  "eo",  "eö",  "oa"
  -- Adverbial: REF CLA CNV IDP BEN TSP CMM COM CNJ UTL
  , "oe",  "öa",  "öe",  "üa|i'",  "üe|ö'",  "üo|î'|û'",  "au",  "eu",  "iu",  "ou"
  -- Relational: ABE CVS COR DEP PVS PTL CON EXC AVR CMP
  , "ëu",  "öu",  "ai'",  "ui'",  "ei'",  "oi'",  "au'",  "iu'",  "eu'",  "ou'"
  -- Temporal1: SML ASS CNR ACS DFF PER PRO PCV PCR ELP ALP INP
  , "a'V",  "e'V",  "i'V",  "o'V",  "u'V",  "ö'V"
  , "ü'|ëu'V",  "ai'V",  "ei'V",  "ui'V",  "oi'V",  "ëi'V"
  -- Temporal2: EPS PLM LMT LOC ORI PSV ALL ABL NAV VOC
  , "au'V",  "eu'V",  "iu'V",  "â'V",  "ê'V",  "ô'V"
  , "î'V|û'V",  "ëu'V",  "ou'V",  "ë"
  -- Compound A: CMP1A-CMP8A
  , "aì",  "eì",  "oì",  "uì",  "aù",  "eù",  "où",  "iù"
  -- Compound B: CMP1B-CMP8B
  , "ao",  "aü",  "eü",  "oü",  "ëì",  "öì",  "ëù",  "öù"
  -- Compound C: CMP1C-CMP8C
  , "eai",  "oai",  "eau",  "oau",  "uai|iau",  "uei|ieu",  "uoi|iou",  "uëi|iëu"
  ]

vcEntries :: [(Case, [Text])]
vcEntries = zip allCases (map splitAlts vcRaw)

vcTable :: Map Case [Text]
vcTable = Map.fromList vcEntries

vcTableReverse :: Map Text Case
vcTableReverse = Map.fromList
  [(v, k) | (k, vs) <- vcEntries, v <- vs]

--------------------------------------------------------------------------------
-- CiVi table: Illocution × Mood → consonant+vowel
-- 6 illocutions × 8 moods = 48, but last 7 don't exist → 41 entries
--------------------------------------------------------------------------------

civiRaw :: [Text]
civiRaw =
  -- ASR × (FAC,SUB,ASM,SPC,COU,HYP,IPL,ASC)
  [ "|wë", "wa", "yë", "ya", "yû", "hë", "ha", "hû|hî"
  -- DIR × (FAC..ASC)
  , "we", "wö", "ye", "yö", "yeu|wei", "he", "hö", "hei"
  -- IRG × (FAC..ASC)
  , "wu", "wâ", "yu", "yâ", "yau|wai", "hu", "hâ", "hai"
  -- ADM × (FAC..ASC)
  , "wo", "wê", "yo", "yê", "you|woi", "ho", "hê", "hoi"
  -- HOR × (FAC..ASC)
  , "wi", "wô", "yi", "yô", "yiu|wui", "hi", "hô", "hui"
  -- DEC × FAC only (remaining 7 combos don't exist)
  , "wî"
  ]

civiKeys :: [SlotCiVi]
civiKeys = take 41
  [(ill, mood) | ill <- allOf, mood <- allOf]

civiEntries :: [(SlotCiVi, [Text])]
civiEntries = zip civiKeys (map splitAlts civiRaw)

civiTable :: Map SlotCiVi [Text]
civiTable = Map.fromList civiEntries

civiTableReverse :: Map Text SlotCiVi
civiTableReverse = Map.fromList
  [(v, k) | (k, vs) <- civiEntries, v <- vs]

--------------------------------------------------------------------------------
-- Vf table: Context × Format → vowel
-- 4 contexts × 10 formats = 40 entries
--------------------------------------------------------------------------------

vfRaw :: [Text]
vfRaw =
  -- EXS × (NOF,SCH,ISR,ATH,RSL,SBQ,CCM,OBJ,PRT,AFI)
  [ "|a", "o", "ai", "â", "au", "ëi", "oa", "ea", "aì", "aù"
  -- FNC × formats
  , "i", "ö", "ui", "ae", "iu", "ëu", "oe", "ia|ua", "uì", "iù"
  -- RPS × formats
  , "e", "ü|öe", "ei", "ê", "eu", "öi", "eo", "ie|ue", "eì", "eù"
  -- AMG × formats
  , "u", "öa", "oi", "ô", "ou", "öu", "iö|uö", "io|uo", "oì", "où"
  ]

vfKeys :: [SlotVf]
vfKeys = [(ctx, fmt) | ctx <- allOf, fmt <- allOf]

vfEntries :: [(SlotVf, [Text])]
vfEntries = zip vfKeys (map splitAlts vfRaw)

vfTable :: Map SlotVf [Text]
vfTable = Map.fromList vfEntries

vfTableReverse :: Map Text SlotVf
vfTableReverse = Map.fromList
  [(v, k) | (k, vs) <- vfEntries, v <- vs]

--------------------------------------------------------------------------------
-- Cb table: Bias → consonant cluster
-- 49 entries
--------------------------------------------------------------------------------

-- | Glottal stop prefix used in bias consonant clusters (U+2019)
gl :: Text
gl = "\x2019"

cbRaw :: [Text]
cbRaw =
  [ ""                           -- NOB (no marker)
  , gl<>"n",    gl<>"nn"         -- ASU, ASU+
  , gl<>"m",    gl<>"mm"         -- HPB, HPB+
  , gl<>"ň",    gl<>"ňň"        -- COI, COI+
  , gl<>"ţ",    gl<>"ţţ"        -- ACP, ACP+
  , gl<>"ç",    gl<>"çç"        -- RAC, RAC+
  , gl<>"s",    gl<>"ss"         -- STU, STU+
  , gl<>"z",    gl<>"zz"         -- CTV, CTV+
  , gl<>"š",    gl<>"šš"        -- DPV, DPV+
  , gl<>"l",    gl<>"ll"         -- RVL, RVL+
  , gl<>"r",    gl<>"rr"         -- GRT, GRT+
  , gl<>"ř",    gl<>"řř"        -- SOL, SOL+
  , gl<>"ļ",    gl<>"ļļ"        -- SEL, SEL+
  , gl<>"kç",   gl<>"kçç"       -- IRO, IRO+
  , gl<>"pļ",   gl<>"pļļ"       -- EXA, EXA+
  , gl<>"pç",   gl<>"pçç"       -- LTL, LTL+
  , gl<>"x",    gl<>"xx"         -- CRR, CRR+
  , gl<>"xh",   gl<>"xxh"        -- EUP, EUP+
  , gl<>"ks",   gl<>"kss"        -- SKP, SKP+
  , gl<>"f",    gl<>"ff"         -- CYN, CYN+
  , gl<>"kš",   gl<>"kšš"       -- CTP, CTP+
  , gl<>"kf",   gl<>"kff"        -- DSM, DSM+
  , gl<>"pš",   gl<>"pšš"       -- IDG, IDG+
  , gl<>"ps",   gl<>"pss"        -- SGS, SGS+
  , gl<>"pf",   gl<>"pff"        -- PPV, PPV+
  ]

cbEntries :: [(Bias, [Text])]
cbEntries = zip allOf (map (\x -> [x]) cbRaw)

cbTable :: Map Bias [Text]
cbTable = Map.fromList cbEntries

cbTableReverse :: Map Text Bias
cbTableReverse = Map.fromList
  [(v, k) | (k, vs) <- cbEntries, v <- vs]

--------------------------------------------------------------------------------
-- Ca table (loaded from data/v3_ca_table.dat)
-- Essence × Extension × Perspective × Affiliation × Configuration
-- 2 × 6 × 4 × 4 × 9 = 1728 entries
--------------------------------------------------------------------------------

data CaTables = CaTables
  { caForward :: Map CaComplex [Text]
  , caReverse :: Map Text CaComplex
  }
  deriving (Show)

-- | Load Ca tables from a data file.
-- Each line is a consonant cluster (with | for alternatives).
-- Lines correspond to the Cartesian product:
--   Essence × Extension × Perspective × Affiliation × Configuration
-- Both Unicode and ASCII forms are indexed for lookup.
loadCaTables :: FilePath -> IO CaTables
loadCaTables path = do
  contents <- TIO.readFile path
  let lns = T.lines contents
      keys = [ (ess, ext, per, aff, cfg)
             | ess <- allOf, ext <- allOf
             , per <- allOf, aff <- allOf, cfg <- allOf
             ]
      entries = zip keys (map splitAlts lns)
      fwd = Map.fromList entries
      -- Index both Unicode original and ASCII-normalized forms
      rev = Map.fromList
        [ (form, k)
        | (k, vs) <- entries, v <- vs
        , form <- [v, unicodeToAscii v]
        ]
  return CaTables { caForward = fwd, caReverse = rev }

-- | Convert Unicode consonant clusters to ASCII equivalents
-- Handles the common substitutions in V3 Ca data
unicodeToAscii :: Text -> Text
unicodeToAscii = T.concatMap charToAscii
  where
    charToAscii 'ʰ' = "h"    -- modifier small h → h
    charToAscii 'ţ' = "t,"
    charToAscii 'ḑ' = "dh"
    charToAscii 'ç' = "c,"
    charToAscii 'č' = "c^"
    charToAscii 'š' = "s^"
    charToAscii 'ž' = "z^"
    charToAscii 'ň' = "n^"
    charToAscii 'ř' = "r^"
    charToAscii 'ļ' = "l,"
    charToAscii c   = T.singleton c

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

data ParseError
  = UnknownVr Text
  | UnknownVc Text
  | UnknownCa Text
  | UnknownCiVi Text
  | UnknownVf Text
  | UnknownCb Text
  | MalformedWord Text
  deriving (Show, Eq)

-- | Parse a V3 formative from romanized text (without Ca table).
-- Handles: Vr + Cr + Vc
parseFormative :: Text -> Either ParseError Formative
parseFormative word = do
  let w = T.toLower word
  case findVr w of
    Nothing -> Left (MalformedWord word)
    Just (vr, rest) ->
      case splitAtFirstVowel rest of
        Nothing -> Left (MalformedWord word)
        Just (cr, vcRest) ->
          case findVc vcRest of
            Nothing -> Left (UnknownVc vcRest)
            Just (vc, _remaining) ->
              Right (defaultFormative cr)
                { fVr = vr
                , fCase = vc
                }

-- | Parse a V3 formative with full Ca support.
-- Handles: Vr + Cr + Vc (+CiVi) + Ca (+Vf (+Cb))
parseFormativeWithCa :: CaTables -> Text -> Either ParseError Formative
parseFormativeWithCa ca word = do
  let w = T.toLower word
  -- Step 1: Parse Vr (vowel at start)
  (vr, afterVr) <- maybe (Left (MalformedWord word)) Right (findVr w)
  -- Step 2: Parse Cr (consonant root)
  (cr, afterCr) <- maybe (Left (MalformedWord word)) Right (splitAtFirstVowel afterVr)
  -- Step 3: Parse Vc (case vowel), try with optional CiVi
  (vc, civiMaybe, afterVc) <- parseVcCivi afterCr
  -- Step 4: Parse Ca (consonant complex)
  (caVal, afterCa) <- parseCaCluster ca afterVc
  -- Step 5: Parse optional Vf
  (vfMaybe, afterVf) <- parseOptionalVf afterCa
  -- Step 6: Parse optional Cb
  let cbMaybe = if T.null afterVf then Nothing
                else Map.lookup afterVf cbTableReverse
  Right (defaultFormative cr)
    { fVr    = vr
    , fCase  = vc
    , fCiVi  = civiMaybe
    , fCa    = caVal
    , fVf    = vfMaybe
    , fBias  = cbMaybe
    }

-- | Parse Vc, possibly followed by CiVi
parseVcCivi :: Text -> Either ParseError (Case, Maybe SlotCiVi, Text)
parseVcCivi t = do
  -- Try longest Vc match, then check if CiVi follows
  case findVcGreedy t of
    Nothing -> Left (UnknownVc t)
    Just (vc, rest) ->
      -- Try to parse CiVi from the rest
      case findCiVi rest of
        Just (civi, rest') -> Right (vc, Just civi, rest')
        Nothing            -> Right (vc, Nothing, rest)

-- | Find Vc (prefer shorter match if CiVi can follow)
findVcGreedy :: Text -> Maybe (Case, Text)
findVcGreedy t = tryLengths [4, 3, 2, 1] t vcTableReverse

findCiVi :: Text -> Maybe (SlotCiVi, Text)
findCiVi t = tryLengths [3, 2] t civiTableReverse

-- | Parse Ca consonant cluster using the loaded table
parseCaCluster :: CaTables -> Text -> Either ParseError (CaComplex, Text)
parseCaCluster ca t =
  let (cons, rest) = T.span (not . isV) t
  in case Map.lookup cons (caReverse ca) of
       Just caVal -> Right (caVal, rest)
       Nothing    -> Left (UnknownCa cons)

-- | Parse optional Vf slot
parseOptionalVf :: Text -> Either ParseError (Maybe SlotVf, Text)
parseOptionalVf t
  | T.null t  = Right (Nothing, t)
  | otherwise = case tryLengths [3, 2, 1] t vfTableReverse of
      Just (vf, rest) -> Right (Just vf, rest)
      Nothing         -> Right (Nothing, t)  -- not a Vf, might be Cb

-- | Try to match a Vr vowel at the start of the text
findVr :: Text -> Maybe (SlotVr, Text)
findVr t = tryLengths [3, 2, 1] t vrTableReverse

-- | Try to match a Vc vowel at the start of the text
findVc :: Text -> Maybe (Case, Text)
findVc t = tryLengths [4, 3, 2, 1] t vcTableReverse

-- | Try matching prefixes of various lengths against a reverse table
tryLengths :: [Int] -> Text -> Map Text a -> Maybe (a, Text)
tryLengths [] _ _ = Nothing
tryLengths (n:ns) t table
  | T.length t >= n =
      case Map.lookup (T.take n t) table of
        Just val -> Just (val, T.drop n t)
        Nothing  -> tryLengths ns t table
  | otherwise = tryLengths ns t table

-- | Check if a character is a vowel
isV :: Char -> Bool
isV c = c `elem` ("aâäeêëiîoôöuûüáéíóúàèìòùæøɨ" :: [Char])

-- | Split text at the first vowel character (returning consonants, then rest)
splitAtFirstVowel :: Text -> Maybe (Text, Text)
splitAtFirstVowel t =
  let (cons, rest) = T.span (not . isV) t
  in if T.null cons then Nothing
     else Just (cons, rest)
