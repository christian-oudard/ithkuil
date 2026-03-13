{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Word Type Classification
-- Determines whether a word is a formative, adjunct, or referential
module Ithkuil.WordType
  ( WordType(..)
  , ParsedWord(..)
  , classifyWord
  , parseWord
  , glossWord
  , glossWordCompact
  , glossSentence
  , glossSlotVIII
  , glossMoodOrScope
  , extractAffixes
  , extractAllPairs
  , extractVnCn
  , parseOneVnCn
  , classifyDegree
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import Ithkuil.Grammar
import Ithkuil.Parse (splitConjuncts, isVowelChar, parseCase, ParsedFormative(..), parseFormativeReal, ParsedCa(..), isSpecialVv)
import Ithkuil.FullParse (parseVnValence, parseCnMood, parseCnMoodP2, parseCnCaseScope,
                           aspectVowels, phaseVowels)
import Ithkuil.Adjuncts hiding (CarrierAdjunct)
import Ithkuil.Referentials (PersonalRef(..), ReferentEffect(..), refC1All, lookupRefC1, referentLabel)
import Ithkuil.Lexicon (RootEntry(..), AffixEntry(..), lookupRoot, lookupAffix)

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
  | PConcatenated [ParsedFormative]  -- concatenation chain (hyphen-separated)
  | PBias Bias
  | PRegister Register
  | PModular [SlotVIII] Text    -- parsed VnCn pairs, raw text
  | PReferential PersonalRef (Maybe Case) Text  -- referent, parsed case, raw case vowel
  | PAffixual Text Int Text     -- affix Cs, degree, optional scope vowel
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
  | isAffixualAdjunct word = WAffixualAdjunct
  | isReferentialWord word = WReferential
  | otherwise = WFormative

-- | Bias adjuncts are pure consonant clusters (no vowels)
-- Must not be a register adjunct (which also has no vowels)
isBiasAdjunct :: Text -> Bool
isBiasAdjunct word = not (T.null word) && not (T.any isVowelChar word)

-- | Register adjuncts: h + vowel form (per Sec. 8.3)
-- Initial: ha, he, hi, ho, hu
-- Final: hai, hei, hiu, hoi, hui, hü
isRegisterAdjunctWord :: Text -> Bool
isRegisterAdjunctWord word =
  T.toLower word `elem` ["ha", "he", "hi", "ho", "hu",
                          "hai", "hei", "hiu", "hoi", "hui", "hü"]

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
isRefC1 c = c `elem` map snd refC1All

-- | Affixual adjuncts: V-C or V-C-V pattern where C is NOT a Cn consonant
-- Structure: Vx (degree) + Cs (affix consonant) + optional Vs (scope)
isAffixualAdjunct :: Text -> Bool
isAffixualAdjunct word =
  let conjs = splitConjuncts word
  in case conjs of
    [v, c] | not (T.null v) && isVowelChar (T.head v)
             && v /= "ë"
             && not (T.null c) && not (isVowelChar (T.head c))
             && not (isCnConsonant c) -> True
    [v, c, vs] | not (T.null v) && isVowelChar (T.head v)
                 && v /= "ë"
                 && not (T.null c) && not (isVowelChar (T.head c))
                 && not (T.null vs) && isVowelChar (T.head vs) -> True
    _ -> False

-- | Check if a consonant is a Cn (modular adjunct) consonant
isCnConsonant :: Text -> Bool
isCnConsonant c = c `elem` ["h", "hl", "hr", "hm", "hn", "hň",
                              "w", "y", "hw", "hrw", "hmw", "hnw", "hňw"]

-- | Modular adjuncts: short V-C or V-C-V-C pattern with Cn consonant
isModularAdjunct :: Text -> Bool
isModularAdjunct word =
  let conjs = splitConjuncts word
  in case conjs of
    [v, c] | isVowelChar (T.head v) && isCnConsonant c -> True
    [v1, _, _, c2] | isVowelChar (T.head v1) && isCnConsonant c2 -> True
    _ -> False

--------------------------------------------------------------------------------
-- Word Parsing
--------------------------------------------------------------------------------

-- | Parse a word based on its type
parseWord :: Text -> ParsedWord
parseWord word
  -- Handle concatenated formatives (hyphen-separated)
  | "-" `T.isInfixOf` word = parseConcatenatedWord word
  | otherwise = parseSingleWord word

-- | Parse a single (non-concatenated) word
parseSingleWord :: Text -> ParsedWord
parseSingleWord word = case classifyWord word of
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
  WModularAdjunct -> parseModularWord word
  WAffixualAdjunct -> parseAffixualWord word
  WCarrierAdjunct -> parseCarrierWord word
  _ -> PUnparsed word

-- | Parse a concatenated word chain (e.g., "hlamröé-úçtļořëi")
-- All parts except the last must have Cc concatenation marker
-- The last part must not have a concatenation marker
parseConcatenatedWord :: Text -> ParsedWord
parseConcatenatedWord word =
  let parts = T.splitOn "-" word
      parsed = map parseFormativeReal parts
  in case sequence parsed of
    Just pfs | length pfs >= 2 -> PConcatenated pfs
    _ -> PUnparsed word

-- | Parse a register adjunct (initial and final forms)
parseRegister :: Text -> Maybe Register
parseRegister word = parseRegisterLower (T.toLower word)

parseRegisterLower :: Text -> Maybe Register
-- Initial forms
parseRegisterLower "ha"  = Just DSV
parseRegisterLower "he"  = Just PNT
parseRegisterLower "hi"  = Just SPF
parseRegisterLower "ho"  = Just EXM
parseRegisterLower "hu"  = Just CGT
-- Final forms
parseRegisterLower "hai" = Just DSV
parseRegisterLower "hei" = Just PNT
parseRegisterLower "hiu" = Just SPF
parseRegisterLower "hoi" = Just EXM
parseRegisterLower "hui" = Just CGT
parseRegisterLower "hü"  = Just END
parseRegisterLower _     = Nothing

-- | Parse a referential word (simple form: C1-Vc, dual form: C1-C1-Vc)
parseReferentialWord :: Text -> ParsedWord
parseReferentialWord word =
  let conjs = splitConjuncts word
  in case conjs of
    [c, v] -> case lookupRefC1 c of
      Just ref -> PReferential ref (parseCase v) v
      Nothing -> PUnparsed word
    [c1, _c2, v] -> case lookupRefC1 c1 of
      -- For dual referentials, we report just the first referent for now
      Just ref -> PReferential ref (parseCase v) v
      Nothing -> PUnparsed word
    _ -> PUnparsed word

-- | Parse an affixual adjunct word (Vx-Cs or Vx-Cs-Vs)
parseAffixualWord :: Text -> ParsedWord
parseAffixualWord word =
  let conjs = splitConjuncts word
  in case conjs of
    [vx, cs] -> PAffixual cs (classifyDegree vx) ""
    [vx, cs, _vs] -> PAffixual cs (classifyDegree vx) word
    _ -> PUnparsed word

-- | Parse a carrier/quotative/naming adjunct (hl/hm/hn/hň + Vc)
parseCarrierWord :: Text -> ParsedWord
parseCarrierWord word =
  let conjs = splitConjuncts word
  in case conjs of
    (c:rest)
      | not (null rest) ->
        let ct = case c of
              "hl" -> Just CarrierForeign   -- carrier (foreign word)
              "hm" -> Just CarrierQuote     -- quotative
              "hn" -> Just CarrierName      -- naming
              "hň" -> Just CarrierFormula   -- formula
              _    -> Nothing
            content = T.concat rest
        in case ct of
          Just ctype -> PCarrier ctype content
          Nothing -> PUnparsed word
    _ -> PUnparsed word

-- | Parse a modular adjunct word
-- 2-slot form: Vn-Cn (aspect or valence+mood)
-- 4-slot form: Vn-Cn-Vn-Cn (two VnCn pairs)
parseModularWord :: Text -> ParsedWord
parseModularWord word =
  let conjs = splitConjuncts word
  in case conjs of
    [vn, cn] -> case parseOneVnCn vn cn of
      Just s8 -> PModular [s8] word
      Nothing -> PUnparsed word
    [vn1, cn1, vn2, cn2] ->
      let s1 = parseOneVnCn vn1 cn1
          s2 = parseOneVnCn vn2 cn2
          pairs = [s | Just s <- [s1, s2]]
      in if null pairs then PUnparsed word else PModular pairs word
    _ -> PUnparsed word

-- | Parse a single Vn+Cn pair into a SlotVIII value
parseOneVnCn :: Text -> Text -> Maybe SlotVIII
parseOneVnCn vn cn =
  let isP2 = cn `elem` ["w", "y", "hw", "hrw", "hmw", "hnw", "hňw"]
      moodOrScope = case parseCnMood cn of
        Just mood -> Just (MoodVal mood)
        Nothing -> case parseCnMoodP2 cn of
          Just mood -> Just (MoodVal mood)
          Nothing -> case parseCnCaseScope cn of
            Just cs -> Just (CaseScope cs)
            Nothing -> Nothing
  in case moodOrScope of
    Nothing -> Nothing
    Just ms
      | isP2 -> case lookup vn aspectVowels of
          Just asp -> Just (VnCnAspect asp ms)
          Nothing -> Nothing
      | otherwise -> case parseVnValence vn of
          Just val -> Just (VnCnValence val ms)
          Nothing -> case lookup vn phaseVowels of
            Just ph -> Just (VnCnPhase ph ms)
            Nothing -> Nothing

--------------------------------------------------------------------------------
-- Glossing
--------------------------------------------------------------------------------

-- | Gloss a parsed word with lexicon lookup
glossWord :: Map Text RootEntry -> Map Text AffixEntry -> ParsedWord -> Text
glossWord roots affixes pw = case pw of
  PFormative pf -> glossFormative roots affixes pf
  PConcatenated pfs ->
    T.intercalate "—" (map (glossFormative roots affixes) pfs)
  PBias b -> T.pack (show b)
  PRegister r -> T.pack (show r) <> " register"
  PModular pairs _ -> "MOD:" <> T.intercalate "+" (map glossSlotVIII pairs)
  PReferential ref mc vc -> glossReferential ref mc vc
  PAffixual cs deg _ ->
    let abbr = case lookupAffix cs affixes of
          Just entry -> affixAbbrev entry
          Nothing -> cs
    in abbr <> "/" <> T.pack (show deg)
  PCarrier ct _ -> "CARRIER:" <> T.pack (show ct)
  PUnparsed t -> "?" <> t

-- | Gloss a formative with root lookup
-- Omits default values (S1/PRC, STA/BSC/EXS, ASR/OBS) for conciseness
glossFormative :: Map Text RootEntry -> Map Text AffixEntry -> ParsedFormative -> Text
glossFormative roots affixes pf =
  let Root cr = pfRoot pf
      (stem, version) = pfSlotII pf
      (func, spec, ctx) = pfSlotIV pf
      isCsRoot = pfCsRootDegree pf /= Nothing
      -- Root meaning: for Cs-root, show **cs**/degree; for normal, show lexicon entry
      rootMeaning = case pfCsRootDegree pf of
        Just deg -> cr <> "/" <> T.pack (show deg)
        Nothing -> case lookupRoot cr roots of
          Just entry -> "'" <> selectStem stem entry <> "'"
          Nothing -> cr
      -- Stem/Version + Function for Cs-root (combined as "CPT.DYN" etc.)
      -- For normal formatives: stem/version only
      stemVerAbbr = if isCsRoot
        then T.intercalate "." $ filter (not . T.null) $
          [ if version /= PRC then T.pack (show version) else ""
          , if func /= STA then T.pack (show func) else ""
          ]
        else case (stem, version) of
          (S1, PRC) -> ""
          (_, PRC) -> T.pack (show stem)
          (S1, _) -> T.pack (show version)
          _ -> T.pack (show stem) <> "." <> T.pack (show version)
      -- Function/Specification/Context (omit if default STA/BSC/EXS)
      -- For Cs-root, function is already in stemVerAbbr; show context from Vr
      slotIVAbbr = if isCsRoot
        then if ctx /= EXS then T.pack (show ctx) else ""
        else case (func, spec, ctx) of
          (STA, BSC, EXS) -> ""
          _ -> T.intercalate "." $ filter (/= "") $
            [ if func /= STA then T.pack (show func) else ""
            , if spec /= BSC then T.pack (show spec) else ""
            , if ctx /= EXS then T.pack (show ctx) else ""
            ]
      -- Ca complex
      caAbbr = case pfCaParsed pf of
        Just pc -> showCaAbbr pc
        Nothing -> ""
      -- Slot V affixes (CsVx order - note: degree comes from Vx, the second element)
      slotVGlosses = map (\(cs, vx) -> glossOneAffix affixes (vx, cs)) (pfSlotV pf)
      -- Affixes from Ca rest (VxCs pairs after the Ca consonant)
      affixGlosses = map (glossOneAffix affixes) (extractAffixes (pfCa pf))
      -- Slot VIII: VnCn (from pfSlotVIII or extracted from Ca rest)
      slotVIII = case pfSlotVIII pf of
        Just s8 -> Just s8
        Nothing -> case extractVnCn (pfCa pf) of
          Just (vn, cn) -> parseOneVnCn vn cn
          Nothing -> Nothing
      -- Slot IX: Case or Illocution+Validation (omit ASR/OBS default)
      slotIXAbbr = case pfIllocVal pf of
        Just (ASR, OBS) -> "OBS"
        Just (ill, val) ->
          T.pack (show ill) <> "/" <> T.pack (show val)
        Nothing -> case pfCase pf of
          Just c -> T.pack (showCase c)
          Nothing -> ""
      frameAbbr = case pfStress pf of
        Antepenultimate -> "FRA"
        _ -> ""
      concatAbbr = case pfConcatenation pf of
        Just Type1 -> "T1"
        Just Type2 -> "T2"
        Nothing -> ""
  in T.intercalate "-" $ filter (not . T.null)
    [ concatAbbr
    , stemVerAbbr
    , rootMeaning
    , slotIVAbbr
    ] <> slotVGlosses
      <> [caAbbr | not (T.null caAbbr)]
      <> affixGlosses
      <> [glossSlotVIII s8 | Just s8 <- [slotVIII]]
      <> [slotIXAbbr | not (T.null slotIXAbbr)]
      <> [frameAbbr | not (T.null frameAbbr)]

-- | Compact gloss: only shows root meaning and non-default grammatical info
glossWordCompact :: Map Text RootEntry -> Map Text AffixEntry -> ParsedWord -> Text
glossWordCompact roots _affixes pw = case pw of
  PConcatenated pfs ->
    T.intercalate "—" (map (\pf -> glossWordCompact roots _affixes (PFormative pf)) pfs)
  PFormative pf ->
    let Root cr = pfRoot pf
        (stem, _) = pfSlotII pf
        rootMeaning = case lookupRoot cr roots of
          Just entry -> selectStem stem entry
          Nothing -> cr
        caseOrIlloc = case pfIllocVal pf of
          Just (ill, val) | (ill, val) /= (ASR, OBS) ->
            "." <> T.pack (show ill) <> "/" <> T.pack (show val)
          Just _ -> ""  -- ASR/OBS is default, skip
          Nothing -> case pfCase pf of
            Just c -> "." <> T.pack (showCase c)
            Nothing -> ""
        stemMark = case stem of
          S1 -> ""  -- default
          _ -> T.pack (show stem) <> ":"
    in stemMark <> rootMeaning <> caseOrIlloc
  PBias b -> T.pack (show b)
  PRegister r -> T.pack (show r)
  PReferential ref mc _vc -> glossReferential ref mc ""
  PModular pairs _raw -> T.intercalate "+" (map glossSlotVIII pairs)
  PAffixual cs deg _ -> cs <> "/" <> T.pack (show deg)
  PCarrier _ct content -> content
  PUnparsed t -> "?" <> t

-- | Extract VxCs affix pairs from Ca rest conjuncts
-- The first consonant is Ca itself; subsequent V-C pairs are Slot VII affixes
-- If the last pair's consonant is a Cn consonant, it's VnCn (Slot VIII)
extractAffixes :: [Text] -> [(Text, Text)]  -- (Vx vowel, Cs consonant)
extractAffixes parts =
  let allPairs = extractAllPairs parts
  in case allPairs of
    [] -> []
    _ | isCnConsonant (snd (last allPairs)) -> init allPairs
      | otherwise -> allPairs

-- | Extract the VnCn pair from Ca rest conjuncts (if present)
extractVnCn :: [Text] -> Maybe (Text, Text)
extractVnCn parts =
  let allPairs = extractAllPairs parts
  in case allPairs of
    [] -> Nothing
    _ | isCnConsonant (snd (last allPairs)) -> Just (last allPairs)
      | otherwise -> Nothing

-- | Extract all V-C pairs after the first consonant (Ca)
extractAllPairs :: [Text] -> [(Text, Text)]
extractAllPairs [] = []
extractAllPairs parts =
  let afterCa = drop 1 parts
  in pairVC afterCa
  where
    pairVC (v:c:rest)
      | not (T.null v) && isVowelChar (T.head v)
      , not (T.null c) && not (isVowelChar (T.head c))
      = (v, c) : pairVC rest
    pairVC _ = []

-- | Gloss a single affix (Vx, Cs) pair
glossOneAffix :: Map Text AffixEntry -> (Text, Text) -> Text
glossOneAffix affixes (vx, cs) =
  let degree = classifyDegree vx
      abbr = case lookupAffix cs affixes of
        Just entry -> affixAbbrev entry
        Nothing -> cs
  in abbr <> "/" <> T.pack (show degree)

-- | Determine affix degree (1-9, 0) from Vx vowel
classifyDegree :: Text -> Int
classifyDegree v = case lookup v degreeTable of
  Just d -> d
  Nothing -> 0
  where
    degreeTable =
      -- Type 1 (Series 1)
      [ ("a", 1), ("ä", 2), ("e", 3), ("i", 4), ("ëi", 5)
      , ("ö", 6), ("o", 7), ("ü", 8), ("u", 9), ("ae", 0)
      -- Type 2 (Series 2)
      , ("ai", 1), ("au", 2), ("ei", 3), ("eu", 4), ("ëu", 5)
      , ("ou", 6), ("oi", 7), ("iu", 8), ("ui", 9), ("ea", 0)
      -- Type 3 (Series 3)
      , ("ia", 1), ("uä", 1), ("ie", 2), ("uë", 2)
      , ("io", 3), ("üä", 3), ("iö", 4), ("üë", 4), ("eë", 5)
      , ("uö", 6), ("öë", 6), ("uo", 7), ("öä", 7)
      , ("ue", 8), ("ië", 8), ("ua", 9), ("iä", 9), ("üo", 0)
      ]

selectStem :: Stem -> RootEntry -> Text
selectStem S0 = rootStem0
selectStem S1 = rootStem1
selectStem S2 = rootStem2
selectStem S3 = rootStem3

-- | Gloss a SlotVIII (VnCn) value
glossSlotVIII :: SlotVIII -> Text
glossSlotVIII (VnCnValence val ms) = T.pack (show val) <> "-" <> glossMoodOrScope ms
glossSlotVIII (VnCnPhase ph ms) = T.pack (show ph) <> "-" <> glossMoodOrScope ms
glossSlotVIII (VnCnAspect asp ms) = T.pack (show asp) <> "-" <> glossMoodOrScope ms

glossMoodOrScope :: MoodOrScope -> Text
glossMoodOrScope (MoodVal m) = T.pack (show m)
glossMoodOrScope (CaseScope cs) = T.pack (show cs)

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

-- | Gloss a referential word
glossReferential :: PersonalRef -> Maybe Case -> Text -> Text
glossReferential (PersonalRef ref eff) mc _vc =
  let label = referentLabel ref
      effAbbr = case eff of
        NEU -> ""
        BEN -> "/BEN"
        DET -> "/DET"
      caseAbbr = case mc of
        Just c -> "-" <> T.pack (showCase c)
        Nothing -> ""
  in "'" <> label <> "'" <> effAbbr <> caseAbbr

--------------------------------------------------------------------------------
-- Multi-word Sentence Glossing
--------------------------------------------------------------------------------

-- | Parse and gloss a complete sentence (space-separated words)
glossSentence :: Map Text RootEntry -> Map Text AffixEntry -> Text -> Text
glossSentence roots affixes sentence =
  let ws = T.words sentence
      glossedWords = map (\w -> glossWord roots affixes (parseWord w)) ws
  in T.intercalate "  " glossedWords
