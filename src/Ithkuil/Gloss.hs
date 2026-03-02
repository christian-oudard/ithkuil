{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Glossing
-- Produces human-readable morphological glosses at different precision levels.
--
-- Three precision levels (matching IthkuilGloss behavior):
-- - Short: all components abbreviated, root shows generic title
-- - Regular: morphological components abbreviated, affixes written out
-- - Full: all components written out
module Ithkuil.Gloss
  ( Precision(..)
  , glossFormative
  , glossWord
  , glossSentence
  , GlossResult(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Ithkuil.Grammar
import Ithkuil.Parse (splitConjuncts, isVowelChar, isConsonantCluster, vxToDegree)
import Ithkuil.FullParse (ParseResult(..), parseFormative)
import Ithkuil.Adjuncts (parseBias, Bias)
import Ithkuil.Lexicon (RootEntry(..), AffixEntry(..), lookupRoot, lookupAffix)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Precision = Short | Regular | Full
  deriving (Show, Eq, Ord)

data GlossResult = GlossResult
  { grWord    :: Text          -- Original word
  , grGloss   :: Text          -- Gloss string
  , grDetails :: [(Text, Text)] -- Slot-by-slot breakdown
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Word-level Glossing
--------------------------------------------------------------------------------

-- | Gloss a single word from text, detecting word type
glossWord :: Precision -> Map Text RootEntry -> Map Text AffixEntry -> Text -> GlossResult
glossWord prec roots affixes word =
  let conjs = splitConjuncts word
  in case detectWordType conjs of
    BiasWord b -> GlossResult word (T.pack (show b)) [("Bias", T.pack (show b))]
    FormativeWord -> case parseFormative word of
      Success f -> glossFormative prec roots affixes word f
      Failure err -> GlossResult word ("?" <> word <> " [" <> err <> "]") []
    UnknownWord -> GlossResult word ("?" <> word) []

-- | Word type detection from conjuncts
data WordType
  = FormativeWord
  | BiasWord Bias
  | UnknownWord

-- | Detect the type of word from its conjunct structure.
-- Bias adjuncts are single consonant clusters with no vowels.
-- Everything else is attempted as a formative.
detectWordType :: [Text] -> WordType
detectWordType [c] | isConsonantCluster c = case parseBias c of
  Just b  -> BiasWord b
  Nothing -> FormativeWord  -- Could be a single-consonant formative
detectWordType _ = FormativeWord

-- | Gloss a parsed formative
glossFormative :: Precision -> Map Text RootEntry -> Map Text AffixEntry -> Text -> Formative -> GlossResult
glossFormative prec roots affixes word f =
  let Root cr = fSlotIII f
      rootEntry = lookupRoot cr roots
      details = buildDetails prec f rootEntry affixes
      gloss = buildGlossLine prec f rootEntry affixes
  in GlossResult word gloss details

-- | Build the one-line gloss string
buildGlossLine :: Precision -> Formative -> Maybe RootEntry -> Map Text AffixEntry -> Text
buildGlossLine prec f rootEntry affixes =
  let (stem, _) = fSlotII f
      parts = filter (not . T.null)
        [ glossConcatenation prec (fSlotI f)
        , glossStemVersion prec (fSlotII f)
        , glossRoot prec stem (fSlotIII f) rootEntry
        , glossFuncSpecCtx prec (fSlotIV f)
        , glossAffixes prec affixes (fSlotV f)
        , glossCa prec (fSlotVI f)
        , glossAffixes prec affixes (fSlotVII f)
        , glossVnCn prec (fSlotVIII f)
        , glossSlotIX prec (fSlotIX f)
        ]
  in T.intercalate "-" parts

-- | Build slot-by-slot details
buildDetails :: Precision -> Formative -> Maybe RootEntry -> Map Text AffixEntry -> [(Text, Text)]
buildDetails prec f rootEntry affixes = filter (not . T.null . snd)
  [ ("Slot I",    glossConcatenation prec (fSlotI f))
  , ("Slot II",   glossStemVersion prec (fSlotII f))
  , ("Slot III",  glossRootDetail prec (fst (fSlotII f)) (fSlotIII f) rootEntry)
  , ("Slot IV",   glossFuncSpecCtx prec (fSlotIV f))
  , ("Slot V",    glossAffixes prec affixes (fSlotV f))
  , ("Slot VI",   glossCa prec (fSlotVI f))
  , ("Slot VII",  glossAffixes prec affixes (fSlotVII f))
  , ("Slot VIII", glossVnCn prec (fSlotVIII f))
  , ("Slot IX",   glossSlotIX prec (fSlotIX f))
  , ("Stress",    showStress (fStress f))
  ]

--------------------------------------------------------------------------------
-- Slot Glossing
--------------------------------------------------------------------------------

glossConcatenation :: Precision -> Maybe ConcatenationStatus -> Text
glossConcatenation _ Nothing = ""
glossConcatenation Short (Just Type1) = "T1"
glossConcatenation Short (Just Type2) = "T2"
glossConcatenation _ (Just Type1) = "Type-1"
glossConcatenation _ (Just Type2) = "Type-2"

glossStemVersion :: Precision -> SlotII -> Text
glossStemVersion _ (s, v)
  | (s, v) == defaultSlotII = ""  -- Suppress defaults
  | otherwise = showStem s <> "/" <> showVersion v

glossRoot :: Precision -> Stem -> Root -> Maybe RootEntry -> Text
glossRoot Short stem (Root cr) rootEntry =
  "'" <> fromMaybe cr (selectStem stem <$> rootEntry) <> "'"
glossRoot Regular stem (Root cr) rootEntry =
  "'" <> fromMaybe cr (selectStem stem <$> rootEntry) <> "'"
glossRoot Full stem (Root cr) rootEntry =
  "'" <> fromMaybe cr (selectStem stem <$> rootEntry) <> "'"
  <> " [" <> cr <> "]"

selectStem :: Stem -> RootEntry -> Text
selectStem S0 = rootStem0
selectStem S1 = rootStem1
selectStem S2 = rootStem2
selectStem S3 = rootStem3

glossRootDetail :: Precision -> Stem -> Root -> Maybe RootEntry -> Text
glossRootDetail _ stem (Root cr) rootEntry =
  "-" <> cr <> "- " <> fromMaybe "(unknown root)" (selectStem stem <$> rootEntry)

glossFuncSpecCtx :: Precision -> SlotIV -> Text
glossFuncSpecCtx _ (f, s, c)
  | (f, s, c) == defaultSlotIV = ""  -- Suppress defaults
  | otherwise = showFunc f <> "/" <> showSpec s <> "/" <> showCtx c

glossCa :: Precision -> SlotVI -> Text
glossCa _ (co, af, pe, ex, es)
  | (co, af, pe, ex, es) == defaultSlotVI = ""  -- Suppress defaults
  | otherwise = T.intercalate "." $ filter (not . T.null)
    [ if co /= UNI then showConfig co else ""
    , if af /= CSL then showAff af else ""
    , if pe /= M_ then showPer pe else ""
    , if ex /= DEL then showExt ex else ""
    , if es /= NRM then showEss es else ""
    ]

glossAffixes :: Precision -> Map Text AffixEntry -> [Affix] -> Text
glossAffixes _ _ [] = ""
glossAffixes prec affixes affs = T.intercalate "-" $ map (glossAffix prec affixes) affs

glossAffix :: Precision -> Map Text AffixEntry -> Affix -> Text
glossAffix prec affixes a =
  let cs = affixConsonant a
      vx = affixVowel a
      entry = lookupAffix cs affixes
      degreeInfo = vxToDegree vx
  in case (prec, entry, degreeInfo) of
    -- Short: always abbreviation/degree
    (Short, Just e, Just (deg, _)) ->
      affixAbbrev e <> "/" <> T.pack (show deg)
    (Short, _, Just (deg, _)) ->
      cs <> "/" <> T.pack (show deg)
    -- Regular: description if available
    (Regular, Just e, Just (deg, _)) ->
      let descs = affixDegrees e
      in if deg >= 1 && deg <= length descs
         then "'" <> (descs !! (deg - 1)) <> "'"
         else affixAbbrev e <> "/" <> T.pack (show deg)
    -- Full: description with Cs reference
    (Full, Just e, Just (deg, _)) ->
      let descs = affixDegrees e
      in if deg >= 1 && deg <= length descs
         then "'" <> (descs !! (deg - 1)) <> "' [" <> affixAbbrev e <> "/" <> T.pack (show deg) <> "]"
         else affixAbbrev e <> "/" <> T.pack (show deg) <> " [" <> cs <> "]"
    -- Fallback: raw Cs:Vx
    _ -> cs <> ":" <> vx

glossVnCn :: Precision -> Maybe (Valence, MoodOrScope) -> Text
glossVnCn _ Nothing = ""
glossVnCn _ (Just (val, ms))
  | val == MNO && ms == MoodVal FAC = ""  -- Suppress defaults
  | otherwise = T.intercalate "." $ filter (not . T.null)
    [ if val /= MNO then showValence val else ""
    , showMoodOrScope ms
    ]

glossSlotIX :: Precision -> Either Case FormatOrIV -> Text
glossSlotIX _ (Left (Transrelative THM)) = ""  -- Suppress default
glossSlotIX Short (Left c) = showCaseAbbr c
glossSlotIX Regular (Left c) = showCaseAbbr c
glossSlotIX Full (Left c) = showCaseFull c
glossSlotIX _ (Right Format) = ""
glossSlotIX _ (Right (IllocVal ill val)) =
  showIllocution ill <> "/" <> showValidation val

showStress :: Stress -> Text
showStress Penultimate = "penultimate"
showStress Ultimate = "ultimate"
showStress Antepenultimate = "antepenultimate"

--------------------------------------------------------------------------------
-- Sentence Glossing
--------------------------------------------------------------------------------

-- | Gloss a full sentence (space-separated words)
glossSentence :: Precision -> Map Text RootEntry -> Map Text AffixEntry -> Text -> [GlossResult]
glossSentence prec roots affixes sentence =
  map (glossWord prec roots affixes) (T.words sentence)

--------------------------------------------------------------------------------
-- Show Helpers
--------------------------------------------------------------------------------

showStem :: Stem -> Text
showStem S1 = "S1"
showStem S2 = "S2"
showStem S3 = "S3"
showStem S0 = "S0"

showVersion :: Version -> Text
showVersion PRC = "PRC"
showVersion CPT = "CPT"

showFunc :: Function -> Text
showFunc STA = "STA"
showFunc DYN = "DYN"

showSpec :: Specification -> Text
showSpec BSC = "BSC"
showSpec CTE = "CTE"
showSpec CSV = "CSV"
showSpec OBJ = "OBJ"

showCtx :: Context -> Text
showCtx EXS = "EXS"
showCtx FNC = "FNC"
showCtx RPS = "RPS"
showCtx AMG = "AMG"

showConfig :: Configuration -> Text
showConfig = T.pack . show

showAff :: Affiliation -> Text
showAff = T.pack . show

showPer :: Perspective -> Text
showPer M_ = "M"
showPer G_ = "G"
showPer N_ = "N"
showPer A_ = "A"

showExt :: Extension -> Text
showExt = T.pack . show

showEss :: Essence -> Text
showEss = T.pack . show

showValence :: Valence -> Text
showValence = T.pack . show

showMoodOrScope :: MoodOrScope -> Text
showMoodOrScope (MoodVal FAC) = ""
showMoodOrScope (MoodVal m) = T.pack (show m)
showMoodOrScope (CaseScope cs) = T.pack (show cs)

showIllocution :: Illocution -> Text
showIllocution = T.pack . show

showValidation :: Validation -> Text
showValidation = T.pack . show

showCaseAbbr :: Case -> Text
showCaseAbbr (Transrelative c) = T.pack (show c)
showCaseAbbr (Appositive c) = T.pack (show c)
showCaseAbbr (Associative c) = T.pack (show c)
showCaseAbbr (Adverbial c) = T.pack (show c)
showCaseAbbr (Relational c) = T.pack (show c)
showCaseAbbr (Affinitive c) = T.pack (show c)
showCaseAbbr (SpatioTemporal1 c) = T.pack (show c)
showCaseAbbr (SpatioTemporal2 c) = T.pack (show c)

showCaseFull :: Case -> Text
showCaseFull (Transrelative THM) = "Thematic"
showCaseFull (Transrelative INS) = "Instrumental"
showCaseFull (Transrelative ABS) = "Absolutive"
showCaseFull (Transrelative AFF) = "Affective"
showCaseFull (Transrelative STM) = "Stimulative"
showCaseFull (Transrelative EFF) = "Effectuative"
showCaseFull (Transrelative ERG) = "Ergative"
showCaseFull (Transrelative DAT) = "Dative"
showCaseFull (Transrelative IND) = "Inducive"
showCaseFull (Appositive POS) = "Possessive"
showCaseFull (Appositive PRP) = "Proprietive"
showCaseFull (Appositive GEN) = "Genitive"
showCaseFull (Appositive ATT) = "Attributive"
showCaseFull (Appositive PDC) = "Productive"
showCaseFull (Appositive ITP) = "Interpretive"
showCaseFull (Appositive OGN) = "Originative"
showCaseFull (Appositive IDP) = "Interdependent"
showCaseFull (Appositive PAR) = "Partitive"
showCaseFull (Associative APL) = "Applicative"
showCaseFull (Associative PUR) = "Purposive"
showCaseFull (Associative TRA) = "Transmissive"
showCaseFull (Associative DFR) = "Deferential"
showCaseFull (Associative CRS) = "Contrastive"
showCaseFull (Associative TSP) = "Transpositive"
showCaseFull (Associative CMM) = "Commutative"
showCaseFull (Associative CMP) = "Comparative"
showCaseFull (Associative CSD) = "Considerative"
showCaseFull (Adverbial FUN) = "Functive"
showCaseFull (Adverbial TFM) = "Transformative"
showCaseFull (Adverbial CLA) = "Classificative"
showCaseFull (Adverbial RSL) = "Resultative"
showCaseFull (Adverbial CSM) = "Consumptive"
showCaseFull (Adverbial CON) = "Concessive"
showCaseFull (Adverbial AVR) = "Aversive"
showCaseFull (Adverbial CVS) = "Conversive"
showCaseFull (Adverbial SIT) = "Situative"
showCaseFull c = showCaseAbbr c  -- Fallback to abbreviation
