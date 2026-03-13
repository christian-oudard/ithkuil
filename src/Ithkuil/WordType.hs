{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Word Type Classification
-- Determines whether a word is a formative, adjunct, or referential
module Ithkuil.WordType
  ( WordType(..)
  , ParsedWord(..)
  , classifyWord
  , parseWord
  , glossWord
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Ithkuil.Grammar
import Ithkuil.Parse (splitConjuncts, isVowelChar, ParsedFormative(..), parseFormativeReal, ParsedCa(..))
import Ithkuil.Adjuncts
import Ithkuil.Referentials (PersonalRef(..), refC1)
import Ithkuil.Lexicon (RootEntry(..), AffixEntry(..), lookupRoot)

--------------------------------------------------------------------------------
-- Word Type Classification
--------------------------------------------------------------------------------

-- | The type of an Ithkuil word
data WordType
  = WFormative          -- ^ Standard formative (noun/verb)
  | WBiasAdjunct        -- ^ Pure-consonant bias adjunct
  | WRegisterAdjunct    -- ^ h-initial register adjunct
  | WModularAdjunct     -- ^ Modular adjunct (VnCn pattern)
  | WAffixualAdjunct    -- ^ Affixual adjunct
  | WReferential        -- ^ Personal referential
  | WCarrierAdjunct     -- ^ Carrier/quotative/naming adjunct
  | WUnknown            -- ^ Could not classify
  deriving (Show, Eq, Ord)

-- | A parsed word with its classification
data ParsedWord
  = PFormative ParsedFormative
  | PBias Bias
  | PRegister Register
  | PModular Text Text          -- Vn, Cn raw forms
  | PReferential PersonalRef Text  -- referent, case vowel
  | PCarrier CarrierType Text   -- carrier type, content
  | PUnparsed Text              -- Could not parse
  deriving (Show, Eq)

-- | Classify a word based on its phonological structure
classifyWord :: Text -> WordType
classifyWord word
  | T.null word = WUnknown
  | isRegisterAdjunctWord word = WRegisterAdjunct
  | isCarrierAdjunct word = WCarrierAdjunct
  | isBiasAdjunct word = WBiasAdjunct
  | isModularAdjunct word = WModularAdjunct
  | isReferentialWord word = WReferential
  | otherwise = WFormative

-- | Bias adjuncts are pure consonant clusters (no vowels)
-- Must not be a register adjunct (which also has no vowels)
isBiasAdjunct :: Text -> Bool
isBiasAdjunct word = not (T.null word) && not (T.any isVowelChar word)

-- | Register adjuncts: specific h-initial forms
-- hw, hlw, hrw, hmw, hnw, hňw, hww, or bare "h"
isRegisterAdjunctWord :: Text -> Bool
isRegisterAdjunctWord word =
  word `elem` ["hw", "hlw", "hrw", "hmw", "hnw", "hňw", "hww", "h"]

-- | Carrier adjuncts start with hl, hm, hn, or hň followed by vowel
isCarrierAdjunct :: Text -> Bool
isCarrierAdjunct word =
  let conjs = splitConjuncts word
  in case conjs of
    (c:v:_) | c `elem` ["hl", "hm", "hn", "hň"]
              && not (T.null v) && isVowelChar (T.head v) -> True
    _ -> False

-- | Referential words: C1-V or C1-C1-V pattern (2-3 conjuncts only)
-- Formatives have 4+ conjuncts (Vv-Cr-Vr-Ca or Cr-Vr-Ca-Vc)
-- Referentials are distinctly shorter: just C1 + case vowel
isReferentialWord :: Text -> Bool
isReferentialWord word =
  let conjs = splitConjuncts word
  in case conjs of
    -- Simple referential: C1-Vc (just 2 conjuncts)
    [c, v] | not (T.null c) && not (isVowelChar (T.head c))
             && not (T.null v) && isVowelChar (T.head v)
             && isRefC1 c -> True
    -- Dual referential: C1-C1-Vc (3 conjuncts, both consonant)
    [c1, c2, v] | isRefC1 c1 && isRefC1 c2
                  && not (T.null v) && isVowelChar (T.head v) -> True
    _ -> False

-- | Check if a consonant is a valid referential C1
isRefC1 :: Text -> Bool
isRefC1 c = c `elem` map snd refC1List
  where
    refC1List = [(ref, refC1 ref) | ref <- [minBound..maxBound]]

-- | Modular adjuncts: short V-C or V-C-V-C pattern with Cn consonant
isModularAdjunct :: Text -> Bool
isModularAdjunct word =
  let conjs = splitConjuncts word
  in case conjs of
    [v, c] | isVowelChar (T.head v) && isCn c -> True
    [v1, c1, v2, c2] | isVowelChar (T.head v1) && isCn c2 -> True
    _ -> False
  where
    isCn c = c `elem` ["h", "hl", "hr", "hm", "hn", "hň",
                        "w", "y", "hw", "hrw", "hmw", "hnw", "hňw"]

--------------------------------------------------------------------------------
-- Word Parsing
--------------------------------------------------------------------------------

-- | Parse a word based on its type
parseWord :: Text -> ParsedWord
parseWord word = case classifyWord word of
  WBiasAdjunct -> case parseBias word of
    Just b -> PBias b
    Nothing -> PUnparsed word
  WRegisterAdjunct -> case parseRegister word of
    Just r -> PRegister r
    Nothing -> PUnparsed word
  WFormative -> case parseFormativeReal word of
    Just pf -> PFormative pf
    Nothing -> PUnparsed word
  WReferential -> parseReferentialWord word
  _ -> PUnparsed word

-- | Parse a register adjunct
parseRegister :: Text -> Maybe Register
parseRegister "hw"  = Just NRR
parseRegister "hlw" = Just DSV
parseRegister "hrw" = Just PTH
parseRegister "hmw" = Just CGT
parseRegister "hnw" = Just SPF
parseRegister "hňw" = Just EXM
parseRegister "hww" = Just MTR
parseRegister "h"   = Just END
parseRegister _     = Nothing

-- | Parse a referential word (simple form: C1-Vc)
parseReferentialWord :: Text -> ParsedWord
parseReferentialWord word =
  let conjs = splitConjuncts word
  in case conjs of
    (c:v:_) -> case lookupRefC1 c of
      Just ref -> PReferential ref v
      Nothing -> PUnparsed word
    _ -> PUnparsed word

-- | Look up referential C1 consonant
lookupRefC1 :: Text -> Maybe PersonalRef
lookupRefC1 c = case filter (\ref -> refC1 ref == c) [minBound..maxBound] of
  (ref:_) -> Just ref
  [] -> Nothing

--------------------------------------------------------------------------------
-- Glossing
--------------------------------------------------------------------------------

-- | Gloss a parsed word with lexicon lookup
glossWord :: Map Text RootEntry -> Map Text AffixEntry -> ParsedWord -> Text
glossWord roots affixes pw = case pw of
  PFormative pf -> glossFormative roots affixes pf
  PBias b -> T.pack (show b)
  PRegister r -> T.pack (show r) <> " register"
  PModular vn cn -> "MOD:" <> vn <> "/" <> cn
  PReferential ref vc -> T.pack (show ref) <> "." <> vc
  PCarrier ct _ -> "CARRIER:" <> T.pack (show ct)
  PUnparsed t -> "?" <> t

-- | Gloss a formative with root lookup
glossFormative :: Map Text RootEntry -> Map Text AffixEntry -> ParsedFormative -> Text
glossFormative roots _ pf =
  let Root cr = pfRoot pf
      (stem, version) = pfSlotII pf
      (func, spec, ctx) = pfSlotIV pf
      -- Root meaning
      rootMeaning = case lookupRoot cr roots of
        Just entry -> "'" <> selectStem stem entry <> "'"
        Nothing -> cr
      -- Grammatical abbreviations
      stemAbbr = T.pack $ show stem
      verAbbr = T.pack $ show version
      funcAbbr = T.pack $ show func
      specAbbr = T.pack $ show spec
      ctxAbbr = T.pack $ show ctx
      -- Ca complex
      caAbbr = case pfCaParsed pf of
        Just pc -> showCaAbbr pc
        Nothing -> ""
      -- Case
      caseAbbr = case pfCase pf of
        Just c -> "-" <> T.pack (showCase c)
        Nothing -> ""
  in T.intercalate "-" $ filter (not . T.null)
    [ stemAbbr <> "/" <> verAbbr
    , rootMeaning
    , funcAbbr <> "/" <> specAbbr <> "/" <> ctxAbbr
    , caAbbr
    ] <> [caseAbbr | not (T.null caseAbbr)]

selectStem :: Stem -> RootEntry -> Text
selectStem S0 = rootStem0
selectStem S1 = rootStem1
selectStem S2 = rootStem2
selectStem S3 = rootStem3

showCaAbbr :: ParsedCa -> Text
showCaAbbr pc
  | pc == ParsedCa UNI CSL M_ DEL NRM = ""  -- Default, don't show
  | otherwise = T.intercalate "/" $ filter (/= "") $
      [ if pcConfig pc /= UNI then T.pack (show (pcConfig pc)) else ""
      , if pcAffiliation pc /= CSL then T.pack (show (pcAffiliation pc)) else ""
      , if pcPerspective pc /= M_ then showPersp (pcPerspective pc) else ""
      , if pcExtension pc /= DEL then T.pack (show (pcExtension pc)) else ""
      , if pcEssence pc /= NRM then T.pack (show (pcEssence pc)) else ""
      ]
  where
    showPersp M_ = "M"
    showPersp G_ = "G"
    showPersp N_ = "N"
    showPersp A_ = "A"

showCase :: Case -> String
showCase (Transrelative c) = show c
showCase (Appositive c) = show c
showCase (Associative c) = show c
showCase (Adverbial c) = show c
showCase (Relational c) = show c
showCase (Affinitive c) = show c
showCase (SpatioTemporal1 c) = show c
showCase (SpatioTemporal2 c) = show c

--------------------------------------------------------------------------------
-- Multi-word Sentence Glossing
--------------------------------------------------------------------------------

-- | Parse and gloss a complete sentence (space-separated words)
glossSentence :: Map Text RootEntry -> Map Text AffixEntry -> Text -> Text
glossSentence roots affixes sentence =
  let words = T.words sentence
      glossedWords = map (\w -> glossWord roots affixes (parseWord w)) words
  in T.intercalate "  " glossedWords
