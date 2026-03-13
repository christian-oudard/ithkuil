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
  , glossSentenceWords
  , isCarrierParsed
  , glossSlotVIII
  , glossMoodOrScope
  , extractAffixes
  , extractAllPairs
  , extractVnCn
  , parseOneVnCn
  , classifyDegree
  , glossCz
  , glossVz
  , glossOneAffix
  , validateSlotVMarker
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import Ithkuil.Grammar
import Data.Maybe (isJust)
import Ithkuil.Phonology (vowelFormLookup)
import Ithkuil.Parse (splitConjuncts, isVowelChar, parseCase, parseCa, ParsedFormative(..), parseFormativeReal, ParsedCa(..), normalizeAccents, detectStressSimple, isGeminateCa, isSpecialVv)
import Ithkuil.FullParse (parseVnValence, parseCnMood, parseCnMoodP2, parseCnCaseScope,
                           aspectVowels, phaseVowels, levelVowels, effectVowels)
import Ithkuil.Adjuncts hiding (CarrierAdjunct)
import Ithkuil.Referentials (PersonalRef(..), ReferentEffect(..), refC1All, decomposeRefCluster, referentAbbrev)
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
  | WMultipleAffixAdj   -- ^ Multiple affix adjunct (Cs-Vx-Cz-VxCs...)
  | WReferential        -- ^ Personal referential
  | WCarrierAdjunct     -- ^ Carrier/quotative/naming adjunct
  | WCombinationRef     -- ^ Combination referential (C1-Vc-Spec-VxCs-Vc2)
  | WMoodCaseScopeAdj   -- ^ Mood/case-scope adjunct (hr + vowel)
  | WUnknown            -- ^ Could not classify
  deriving (Show, Eq, Ord)

-- | A parsed word with its classification
data ParsedWord
  = PFormative ParsedFormative
  | PConcatenated [ParsedFormative]  -- concatenation chain (hyphen-separated)
  | PBias Bias
  | PRegister Register
  | PModular [SlotVIII] Text Text  -- parsed VnCn pairs, final vowel gloss, raw text
  | PReferential [PersonalRef] (Maybe Case) Text (Maybe (Text, Maybe Case, Maybe PersonalRef))
    -- ^ referent(s) from C1 cluster, case, raw case vowel, optional (scope w/y, case2, ref2)
  | PAffixual Text Int Int Text     -- affix Cs, degree, type (1-3), optional scope vowel
  | PMultipleAffix (Text, Text) Text [(Text, Text)] (Maybe Text)
    -- ^ first affix (Vx,Cs), Cz scope, additional affixes (Vx,Cs), optional Vz scope
  | PCarrier CarrierType Text   -- carrier type, content
  | PCombinationRef [PersonalRef] (Maybe Case) Text [(Text, Text)] (Maybe Case)
    -- ^ referent(s), case1, spec, affixes (VxCs), case2
  | PMoodCaseScope MoodOrScope  -- standalone mood/case-scope adjunct
  | PError Text Text            -- error message, original word
  | PUnparsed Text              -- Could not parse
  deriving (Show, Eq)

-- | Classify a word based on its phonological structure
classifyWord :: Text -> WordType
classifyWord word
  | T.null word = WUnknown
  | isBiasAdjunct word = WBiasAdjunct
  | isMoodCaseScopeAdjunct word = WMoodCaseScopeAdj
  | isRegisterAdjunctWord word = WRegisterAdjunct
  | isCarrierAdjunct word = WCarrierAdjunct
  | isModularAdjunct word = WModularAdjunct
  | isCombinationRef word = WCombinationRef
  | isMultipleAffixAdjunct word = WMultipleAffixAdj
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

-- | Referential words: [ë/äi] C1 [ëC1]* V [w/y V [C1 [ë]]]
-- C1 must be a referential consonant (or decomposable cluster), w/y marks RPV/perspective scope
isReferentialWord :: Text -> Bool
isReferentialWord word =
  let conjs = splitConjuncts word
      -- Strip optional ë/äi prefix
      rest0 = case conjs of
        ("ë":cs)  -> cs
        ("äi":cs) -> cs
        cs        -> cs
      -- Consume referential consonants (C1 [ë C1]*)
      -- A consonant is valid if it's a single ref C1 or decomposes into refs
      consumeRefs (c:cs)
        | isRefCluster c = case cs of
            ("ë":c2:cs2) | isRefCluster c2 -> consumeRefs (c2:cs2)
            _ -> Just cs
        | otherwise = Nothing
      consumeRefs [] = Nothing
      -- After C1s, expect: V [w/y V [C1 [ë]]]
      isValidTail cs = case cs of
        [v]            | isV v -> True
        [v, wy, v2]   | isV v && wy `elem` ["w", "y"] && isV v2 -> True
        [v, wy, v2, c] | isV v && wy `elem` ["w", "y"] && isV v2 && isRefCluster c -> True
        _ -> False
      isV t = not (T.null t) && isVowelChar (T.head t)
  in case consumeRefs rest0 of
    Just tail' -> isValidTail tail'
    Nothing -> False

-- | Check if a consonant is a valid referential C1 (single or cluster)
isRefC1 :: Text -> Bool
isRefC1 c = c `elem` map snd refC1All || c == "ļ"

-- | Check if a consonant cluster decomposes into valid referential consonants
isRefCluster :: Text -> Bool
isRefCluster c = isRefC1 c || case decomposeRefCluster c of
  Just (_:_) -> True
  _ -> False

-- | Mood/case-scope adjuncts: "hr" + vowel (Sec. 8.5)
-- These set standalone mood or case-scope for the following formative
isMoodCaseScopeAdjunct :: Text -> Bool
isMoodCaseScopeAdjunct word =
  let conjs = splitConjuncts word
  in case conjs of
    [c, v] | c == "hr"
             && not (T.null v) && isVowelChar (T.head v) -> True
    _ -> False

-- | Combination referentials: [ë] C1 Vc Spec [VxCs...] [Vc2]
-- Spec must be x/xt/xp/xx; C1 must be a referential consonant
-- No geminate consonants (gemination signals formative Ca boundary)
isCombinationRef :: Text -> Bool
isCombinationRef word =
  let conjs = splitConjuncts word
      -- Strip optional ë prefix
      rest0 = case conjs of
        ("ë":cs) -> cs
        cs       -> cs
      specConsonants = ["x", "xt", "xp", "xx"] :: [Text]
      -- Check that no consonant after spec is geminated
      noGeminates xs = not $ any (\x -> not (T.null x) && not (isVowelChar (T.head x)) && isGeminateCa x) xs
  in case rest0 of
    (c:v:spec:rest) | isRefCluster c
                    , not (T.null v) && isVowelChar (T.head v)
                    , spec `elem` specConsonants
                    , noGeminates rest -> True
    -- "a" + CP consonant form
    ("a":cp:v:spec:rest) | cp `elem` ["hl", "hm", "hn", "hň"]
                         , not (T.null v) && isVowelChar (T.head v)
                         , spec `elem` specConsonants
                         , noGeminates rest -> True
    _ -> False

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

-- | Cz consonants for multiple affix adjuncts (scope of first affix)
isCzConsonant :: Text -> Bool
isCzConsonant c = c `elem` (["h", "'h", "'hl", "'hr", "hw", "'hw"] :: [Text])

-- | Multiple affix adjuncts: [ë] Cs Vx Cz (VxCs)+ [Vz]
-- Must have a Cz consonant between first affix and subsequent affixes
isMultipleAffixAdjunct :: Text -> Bool
isMultipleAffixAdjunct word =
  let conjs = splitConjuncts word
      -- Strip optional ë prefix
      rest = case conjs of
        ("ë":cs) -> cs
        cs       -> cs
  in case rest of
    -- Cs Vx Cz Vx2 Cs2 [tail...]
    (cs:vx:cz:vx2:cs2:_)
      | not (T.null cs) && not (isVowelChar (T.head cs))
      , not (T.null vx) && isVowelChar (T.head vx)
      , isCzConsonant cz
      , not (T.null vx2) && isVowelChar (T.head vx2)
      , not (T.null cs2) && not (isVowelChar (T.head cs2))
      -> True
    _ -> False

-- | Check if a consonant is a Cn (modular adjunct) consonant
isCnConsonant :: Text -> Bool
isCnConsonant c = c `elem` ["h", "hl", "hr", "hm", "hn", "hň",
                              "w", "y", "hw", "hrw", "hmw", "hnw", "hňw"]

-- | Modular adjuncts: [w/y] (V Cn){0-3} V pattern
-- Always ends with a vowel; all consonants must be Cn consonants
isModularAdjunct :: Text -> Bool
isModularAdjunct word =
  let conjs = splitConjuncts word
      -- Strip optional w/y prefix
      rest = case conjs of
        (c:cs) | c == "w" || c == "y" -> cs
        cs -> cs
      -- Must end with a vowel and have alternating V Cn V Cn ... V
      isValidPattern [] = False
      isValidPattern [v] = not (T.null v) && isVowelChar (T.head v)
                        && v /= "ë" && v /= "äi"
      isValidPattern (v:c:xs)
        | not (T.null v) && isVowelChar (T.head v)
        , not (T.null c) && not (isVowelChar (T.head c))
        , isCnConsonant c = isValidPattern xs
        | otherwise = False
      nCnPairs = (length rest - 1) `div` 2
  in isValidPattern rest && nCnPairs <= 3

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
  WFormative -> case detectFormativeError word of
    Just err -> PError err word
    Nothing -> case parseFormativeReal word of
      Just pf -> PFormative pf
      Nothing -> PUnparsed word
  WReferential -> parseReferentialWord word
  WModularAdjunct -> parseModularWord word
  WMultipleAffixAdj -> parseMultipleAffixWord word
  WAffixualAdjunct -> parseAffixualWord word
  WCarrierAdjunct -> parseCarrierWord word
  WCombinationRef -> parseCombinationRefWord word
  WMoodCaseScopeAdj -> parseMoodCaseScopeAdj word
  _ -> PUnparsed word

-- | Detect structural errors in formative words before parsing
-- Returns Just errorMessage if an error is detected
detectFormativeError :: Text -> Maybe Text
detectFormativeError word =
  let lw = T.toLower word
      -- Strip ç prefix for analysis
      stripped = case T.uncons lw of
        Just ('ç', rest) -> case T.uncons rest of
          Just ('ë', rest2) | not (T.null rest2) -> rest2
          Just ('ç', rest2) -> "y" <> rest2
          _ | not (T.null rest) -> rest
          _ -> lw
        _ -> lw
      sparts = splitConjuncts stripped
  in case sparts of
    -- Shortcut (w/y) + Cs-root Vv (ëi, eë, ëu, oë)
    (cc:vv:_) | cc `elem` ["w", "y"]
              , isSpecialVv vv -> Just "Shortcuts can't be used with a Cs-root"
    _ -> Nothing

-- | Parse a concatenated word chain (e.g., "hlamröé-úçtļořëi")
-- All parts except the last must have Cc concatenation marker
-- The last part must not have a concatenation marker
-- Sentence prefix (ç) may only appear on the first part
parseConcatenatedWord :: Text -> ParsedWord
parseConcatenatedWord word =
  let parts = T.splitOn "-" word
      -- Check for sentence prefix in non-first parts
      hasInnerSentencePrefix = any (\p -> case T.uncons (T.toLower p) of
        Just ('ç', _) -> True; _ -> False) (drop 1 parts)
      -- Check for glottal stops in non-final parts (invalid in concatenated formatives)
      nonFinalParts = if length parts > 1 then init parts else []
      hasGlottalInConcat = any ("'" `T.isInfixOf`) nonFinalParts
      parsed = map parseFormativeReal parts
  in if hasInnerSentencePrefix
    then PError "Sentence prefix inside concatenation chain" word
    else if hasGlottalInConcat
    then PError "Unexpected glottal stop in concatenated formative" word
    else case sequence parsed of
      Just pfs
        | length pfs >= 2
        -> PConcatenated pfs
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

-- | Parse a referential word
-- Simple: C1-Vc, Dual: C1-ë-C1-Vc, Extended: C1-Vc-w/y-Vc2
-- C1 may be a consonant cluster decomposable into multiple referents
parseReferentialWord :: Text -> ParsedWord
parseReferentialWord word =
  let conjs = splitConjuncts word
      stress = detectStressSimple word
      -- Strip optional ë/äi prefix
      rest0 = case conjs of
        ("ë":cs)  -> cs
        ("äi":cs) -> cs
        cs        -> cs
      -- Consume C1 referential consonants, collecting refs
      -- Supports cluster decomposition (e.g., "ţn" → [Rmi.BEN, R2p.NEU])
      consumeRefs (c:cs)
        | Just refs' <- decomposeRefCluster c
        , not (null refs') = case cs of
            ("ë":c2:cs2) -> case consumeRefs (c2:cs2) of
              (moreRefs, rest') -> (refs' ++ moreRefs, rest')
            _ -> (refs', cs)
        | otherwise = ([], c:cs)
      consumeRefs [] = ([], [])
      (refs, tail') = consumeRefs rest0
      nv = normalizeAccents
      -- Ultimate stress = RPV essence; encode as vc suffix
      essenceTag = case stress of { Ultimate -> "\\RPV"; _ -> "" }
  in case (refs, tail') of
    (_:_, [v]) ->
      PReferential refs (parseCase (nv v)) (v <> essenceTag) Nothing
    (_:_, [v, wy, v2]) | wy `elem` ["w", "y"] ->
      PReferential refs (parseCase (nv v)) (v <> essenceTag) (Just (wy, parseCase (nv v2), Nothing))
    (_:_, [v, wy, v2, c2]) | wy `elem` ["w", "y"], isRefCluster c2 ->
      let ref2 = case decomposeRefCluster c2 of
            Just (r:_) -> Just r
            _ -> Nothing
      in PReferential refs (parseCase (nv v)) (v <> essenceTag) (Just (wy, parseCase (nv v2), ref2))
    _ -> PUnparsed word

-- | Parse an affixual adjunct word (Vx-Cs or Vx-Cs-Vs)
parseAffixualWord :: Text -> ParsedWord
parseAffixualWord word =
  let conjs = splitConjuncts word
  in case conjs of
    [vx, cs] -> let (deg, atype) = classifyDegreeType vx in PAffixual cs deg atype ""
    [vx, cs, vs] -> let (deg, atype) = classifyDegreeType vx in PAffixual cs deg atype (glossVz vs)
    _ -> PUnparsed word

-- | Parse a multiple affix adjunct: [ë] Cs Vx Cz (VxCs)+ [Vz]
parseMultipleAffixWord :: Text -> ParsedWord
parseMultipleAffixWord word =
  let conjs = splitConjuncts word
      -- Strip optional ë prefix
      rest = case conjs of
        ("ë":cs) -> cs
        cs       -> cs
  in case rest of
    (cs:vx:cz:tail')
      | isCzConsonant cz ->
        let firstAffix = (normalizeAccents vx, cs)
            (moreAffixes, lastV) = parseVxCsPairsMulti tail'
        in PMultipleAffix firstAffix cz moreAffixes lastV
    _ -> PUnparsed word
  where
    -- Parse VxCs pairs; if odd element remains at end, it's the Vz scope vowel
    parseVxCsPairsMulti [] = ([], Nothing)
    parseVxCsPairsMulti [v]
      | not (T.null v) && isVowelChar (T.head v) = ([], Just v)
    parseVxCsPairsMulti (v:c:more)
      | not (T.null v) && isVowelChar (T.head v)
      , not (T.null c) && not (isVowelChar (T.head c))
      = let (rest', lastV) = parseVxCsPairsMulti more
        in ((normalizeAccents v, c) : rest', lastV)
    parseVxCsPairsMulti _ = ([], Nothing)

-- | Parse a combination referential: [ë] C1 Vc Spec [VxCs...] [Vc2]
parseCombinationRefWord :: Text -> ParsedWord
parseCombinationRefWord word =
  let conjs = splitConjuncts word
      -- Strip optional ë prefix
      rest0 = case conjs of
        ("ë":cs) -> cs
        cs       -> cs
      -- Also handle "a" + CP consonant
      (c1, afterC1) = case rest0 of
        ("a":cp:cs) | cp `elem` ["hl", "hm", "hn", "hň"] -> (cp, cs)
        (c:cs) -> (c, cs)
        _ -> ("", [])
  in case afterC1 of
    (vc:spec:rest) | spec `elem` ["x", "xt", "xp", "xx"] ->
      let refs = case decomposeRefCluster c1 of
            Just rs@(_:_) -> Just rs
            _ -> Nothing
          case1 = parseCase vc
          -- Parse VxCs affix pairs from rest
          (affixPairs, lastV) = parseVxCsPairs rest
          -- Combination referential second case: "a"/Nothing = no case, "üa" = THM
          case2 = lastV >>= parseCaseCombiRef
      in case refs of
        Just rs -> PCombinationRef rs case1 spec affixPairs case2
        Nothing -> PUnparsed word
    _ -> PUnparsed word
  where
    parseVxCsPairs [] = ([], Nothing)
    parseVxCsPairs [v] | not (T.null v) && isVowelChar (T.head v) = ([], Just v)
    parseVxCsPairs (v:c:rest)
      | not (T.null v) && isVowelChar (T.head v)
      , not (T.null c) && not (isVowelChar (T.head c))
      = let (more, lastV) = parseVxCsPairs rest
        in ((v, c) : more, lastV)
    parseVxCsPairs _ = ([], Nothing)
    -- Combination referential case2: "a" = no case, "üa" = THM, else normal
    parseCaseCombiRef "a" = Nothing
    parseCaseCombiRef "üa" = Just (Transrelative THM)
    parseCaseCombiRef v = parseCase v

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

-- | Parse a mood/case-scope adjunct (hr + vowel)
-- Vowel determines mood (Series 1) or case-scope (Series 2)
parseMoodCaseScopeAdj :: Text -> ParsedWord
parseMoodCaseScopeAdj word =
  let conjs = splitConjuncts word
  in case conjs of
    [_, v] -> case parseMcsVowel v of
      Just ms -> PMoodCaseScope ms
      Nothing -> PUnparsed word
    _ -> PUnparsed word
  where
    parseMcsVowel v = case v of
      -- Moods (Series 1 vowels)
      "a"  -> Just (MoodVal FAC)
      "e"  -> Just (MoodVal SUB)
      "i"  -> Just (MoodVal ASM)
      "o"  -> Just (MoodVal SPC)
      "ö"  -> Just (MoodVal COU)
      "u"  -> Just (MoodVal HYP)
      -- Case-scopes (Series 2 vowels)
      "ai" -> Just (CaseScope CCN)
      "ei" -> Just (CaseScope CCA)
      "iu" -> Just (CaseScope CCS)
      "oi" -> Just (CaseScope CCQ)
      "ü"  -> Just (CaseScope CCP)
      "ui" -> Just (CaseScope CCV)
      _    -> Nothing

-- | Parse a modular adjunct word
-- Structure: [w/y] (VnCn){0-3} V(final)
-- V(final) = Aspect (if no VnCn pairs) or Vh scope marker
parseModularWord :: Text -> ParsedWord
parseModularWord word =
  let conjs = splitConjuncts word
      stress = detectStressSimple word
      -- Strip optional w/y scope prefix
      (prefix, rest) = case conjs of
        (c:cs) | c == "w" || c == "y" -> (Just c, cs)
        cs -> (Nothing, cs)
      -- Parse VnCn pairs from rest, leaving final vowel
      parsePairs [] = ([], Nothing)
      parsePairs [v] = ([], Just v)
      parsePairs (vn:cn:xs) = let (more, final) = parsePairs xs
                                  pair = parseOneVnCn vn cn
                              in (pair : more, final)
      (maybePairs, finalV) = parsePairs rest
      pairs = [s | Just s <- maybePairs]
      finalGloss = case finalV of
        Nothing -> ""
        Just v
          | null pairs -> glossAspect v  -- No VnCn pairs → final V is aspect
          | stress == Ultimate -> glossVh v  -- With VnCn pairs + ult stress → Vh scope
          | otherwise -> case parseOneVnCn v "h" of  -- Penult stress → implicit "h" Cn
              Just s8 -> glossSlotVIII s8
              Nothing -> "?" <> v
  in case (prefix, pairs, finalV) of
    (_, [], Just _) | isJust (parseAspect =<< finalV) ->
      PModular [] finalGloss word  -- aspect-only modular adjunct
    (_, _, _) | not (null pairs) ->
      PModular pairs finalGloss word
    _ -> PUnparsed word

-- | Parse an aspect vowel
parseAspect :: Text -> Maybe Aspect
parseAspect v = lookup (normalizeAccents v) aspectVowels

-- | Gloss an aspect vowel
glossAspect :: Text -> Text
glossAspect v = case parseAspect v of
  Just asp -> T.pack (show asp)
  Nothing  -> "?" <> v

-- | Gloss a Cz scope consonant (first affix scope in multiple affix adjunct)
glossCz :: Text -> Text
glossCz cz = case cz of
  "h"    -> "{VDom}"
  "'h"   -> "{VSub}"
  "'hl"  -> "{VIIDom}"
  "'hr"  -> "{VIISub}"
  "hw"   -> "{form.}"
  "'hw"  -> "{concat.}"
  _      -> "?" <> cz

-- | Gloss a Vz scope vowel (scope for additional affixes in multiple affix adjunct)
glossVz :: Text -> Text
glossVz vz = case normalizeAccents vz of
  "a" -> "{VDom}"
  "u" -> "{VSub}"
  "e" -> "{VIIDom}"
  "i" -> "{VIISub}"
  "o" -> "{form.}"
  _   -> "?" <> vz

-- | Gloss a Vh scope marker (final vowel of modular adjunct with VnCn pairs)
glossVh :: Text -> Text
glossVh v = case normalizeAccents v of
  "a" -> "{form.}"
  "e" -> "{mood}"
  "i" -> "{under adj.}"
  "u" -> "{under adj.}"
  "o" -> "{over adj.}"
  _   -> "?" <> v

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
            Nothing -> case lookup vn levelVowels of
              Just lvl -> Just (VnCnLevel lvl False ms)
              Nothing -> case lookup vn effectVowels of
                Just eff -> Just (VnCnEffect eff ms)
                Nothing -> Nothing

--------------------------------------------------------------------------------
-- Glossing
--------------------------------------------------------------------------------

-- | Gloss a parsed word with lexicon lookup
glossWord :: Map Text RootEntry -> Map Text AffixEntry -> ParsedWord -> Text
glossWord roots affixes pw = case pw of
  PFormative pf -> case validateFormative pf of
    Just err -> "Error: " <> err
    Nothing -> glossFormative roots affixes pf
  PConcatenated pfs ->
    case [e | Just e <- map validateFormative pfs] of
      []    -> T.intercalate "—" (map (glossFormative roots affixes) pfs)
      (e:_) -> "Error: " <> e
  PBias b -> let desc = biasGloss b
             in if T.null desc then T.pack (show b) else "\x201C" <> desc <> "\x201D"
  PRegister r -> T.pack (show r)
  PModular pairs fv _ ->
    let glossVnCnDot (VnCnValence val ms) = T.pack (show val) <> "." <> glossMoodOrScope ms
        glossVnCnDot (VnCnPhase ph ms) = T.pack (show ph) <> "." <> glossMoodOrScope ms
        glossVnCnDot (VnCnLevel lvl _abs ms) = T.pack (show lvl) <> "." <> glossMoodOrScope ms
        glossVnCnDot (VnCnEffect eff ms) = T.pack (show eff) <> "." <> glossMoodOrScope ms
        glossVnCnDot (VnCnAspect asp ms) = T.pack (show asp) <> "." <> glossMoodOrScope ms
    in T.intercalate "-" (map glossVnCnDot pairs)
       <> (if T.null fv then "" else "-" <> fv)
  PReferential refs mc vc ext ->
    let essenceSuffix = if "\\RPV" `T.isSuffixOf` vc then "\\RPV" else ""
    in glossReferentials refs mc vc
       <> maybe "" glossRefExt ext
       <> essenceSuffix
  PAffixual cs deg atype scope ->
    let abbr = case lookupAffix cs affixes of
          Just entry -> affixAbbrev entry
          Nothing -> cs
        typeSub = case atype of { 1 -> "₁"; 2 -> "₂"; 3 -> "₃"; _ -> "" }
    in abbr <> "/" <> T.pack (show deg) <> typeSub
       <> (if T.null scope then "" else "-" <> scope)
  PMultipleAffix first cz moreAfxs mVz ->
    glossOneAffix affixes first <> "-" <> glossCz cz
    <> T.concat (map (\p -> "-" <> glossOneAffix affixes p) moreAfxs)
    <> maybe "" (\vz -> "-" <> glossVz vz) mVz
  PCombinationRef refs mc spec afxs mc2 ->
    glossCombinationRefs refs
    -- case1 shown only when case2 is present (Kotlin: Shown(caseA, condition = caseB != null))
    <> case mc2 of
         Just _ -> maybe "" (\c -> "-" <> T.pack (showCase c)) mc
         Nothing -> ""
    <> (if spec /= "x" then "-" <> spec else "")
    <> T.concat (map (\p -> "-" <> glossOneAffix affixes p) afxs)
    <> maybe "" (\c -> "-" <> T.pack (showCase c)) mc2
  PCarrier ct _ -> "CARRIER:" <> T.pack (show ct)
  PMoodCaseScope ms -> glossMoodOrScope ms
  PError msg _ -> "Error: " <> msg
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
        else let hasImplicit = not (pfHasShortcut pf) && pfVvSeries pf > 1
                 base = case (stem, version) of
                   (S1, PRC) | hasImplicit -> "S1"  -- Show stem when implicit affix present
                   (S1, PRC) -> ""
                   (_, PRC) -> T.pack (show stem)
                   (S1, _) -> T.pack (show version)
                   _ -> T.pack (show stem) <> "." <> T.pack (show version)
             in base
      -- Function/Specification/Context (omit if default STA/BSC/EXS)
      -- For Cs-root, Vr encodes Designation (Dx) + Context
      slotIVAbbr = if isCsRoot
        then let deg = case pfCsRootDegree pf of { Just d -> d; Nothing -> 0 }
                 dStr = "D" <> T.pack (show deg)
                 ctxStr = if ctx /= EXS then "." <> T.pack (show ctx) else ""
             in dStr <> ctxStr
        else case (func, spec, ctx) of
          (STA, BSC, EXS) -> ""
          _ -> T.intercalate "." $ filter (/= "") $
            [ if func /= STA then T.pack (show func) else ""
            , if spec /= BSC then T.pack (show spec) else ""
            , if ctx /= EXS then T.pack (show ctx) else ""
            ]
      -- Ca complex: when shortcut, Ca values are shown with stem; otherwise separate
      caAbbr = if pfHasShortcut pf then ""
               else case pfCaParsed pf of
                 Just pc -> showCaAbbr pc
                 Nothing -> ""
      -- Shortcut Ca values grouped with stem (PRX, RPV, G, N, A perspectives)
      shortcutCaAbbr = if pfHasShortcut pf
        then case pfCaParsed pf of
          Just pc -> showCaAbbr pc
          Nothing -> ""
        else ""
      -- Slot V affixes (CsVx order - note: degree comes from Vx, the second element)
      slotVGlosses = map (\(cs, vx) -> glossOneAffix affixes (vx, cs)) (pfSlotV pf)
      -- Affixes from Ca rest (VxCs pairs after the Ca consonant)
      affixGlosses = map (glossOneAffix affixes) (extractAffixes (pfCa pf))
      -- Slot VIII: VnCn (from pfSlotVIII or extracted from Ca rest)
      -- Supports absolute level: V+y+V+Cn pattern
      -- Disambiguate Mood vs CaseScope based on stress
      stress = pfStress pf
      slotVIII = fmap (disambiguateSlotVIII stress) $ case pfSlotVIII pf of
        Just s8 -> Just s8
        Nothing -> case extractVnCnFull (pfCa pf) of
          Just (vn, cn, absLvl) ->
            let parsed = parseOneVnCn vn cn
            in if absLvl
               then case parsed of
                 Just (VnCnLevel lvl _ ms) -> Just (VnCnLevel lvl True ms)
                 _ -> parsed
               else parsed
          Nothing -> Nothing
      -- Slot IX: Case or Illocution+Validation (omit THM default, keep OBS)
      slotIXAbbr = case pfIllocVal pf of
        Just (ASR, OBS) -> "OBS"
        Just (ASR, val) -> T.pack (show val)
        Just (ill, _) -> T.pack (show ill)  -- Non-ASR illocutions have no validation
        Nothing -> case pfCase pf of
          Just c | c /= Transrelative THM -> T.pack (showCase c)
                 <> if pfStress pf == Antepenultimate then "\\FRA" else ""
          _ -> if pfStress pf == Antepenultimate then "FRA" else ""
      -- Implicit affix from Vv Series 2-4 (when no Cc shortcut)
      -- Series 2 → r/4, Series 3 → t/4, Series 4 → t/5
      implicitAffix = if not (pfHasShortcut pf) && pfVvSeries pf > 1
        then case pfVvSeries pf of
          2 -> let abbr = case lookupAffix "r" affixes of
                     Just entry -> affixAbbrev entry
                     Nothing -> "r"
               in abbr <> "/4"
          3 -> let abbr = case lookupAffix "t" affixes of
                     Just entry -> affixAbbrev entry
                     Nothing -> "t"
               in abbr <> "/4"
          4 -> let abbr = case lookupAffix "t" affixes of
                     Just entry -> affixAbbrev entry
                     Nothing -> "t"
               in abbr <> "/5"
          _ -> ""
        else ""
      -- FRA is now merged into slotIXAbbr with backslash notation
      concatAbbr = case pfConcatenation pf of
        Just Type1 -> "T1"
        Just Type2 -> "T2"
        Nothing -> ""
      sentenceAbbr = if pfSentenceStarter pf then "[sentence:]" else ""
  in T.intercalate "-" $ filter (not . T.null)
    [ sentenceAbbr
    , concatAbbr
    , let sv = T.intercalate "." $ filter (not . T.null)
                 [stemVerAbbr, shortcutCaAbbr, implicitAffix]
      in sv
    , rootMeaning
    , slotIVAbbr
    ] <> slotVGlosses
      <> [if T.null caAbbr && not (null (pfSlotV pf)) then "{Ca}" else caAbbr
         | not (T.null caAbbr) || not (null (pfSlotV pf))]
      <> affixGlosses
      <> [glossSlotVIII s8 | Just s8 <- [slotVIII]]
      <> [slotIXAbbr | not (T.null slotIXAbbr)]

-- | Compact gloss: only shows root meaning and non-default grammatical info
glossWordCompact :: Map Text RootEntry -> Map Text AffixEntry -> ParsedWord -> Text
glossWordCompact roots affixes pw = case pw of
  PConcatenated pfs ->
    T.intercalate "—" (map (\pf -> glossWordCompact roots affixes (PFormative pf)) pfs)
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
            Just c | c /= Transrelative THM -> "." <> T.pack (showCase c)
            _ -> ""
        stemMark = case stem of
          S1 -> ""  -- default
          _ -> T.pack (show stem) <> ":"
        -- Truncate long meanings for compact display
        shortMeaning = if T.length rootMeaning > 25
          then T.take 22 rootMeaning <> "..."
          else rootMeaning
        sentencePrefix = if pfSentenceStarter pf then "[s:]" else ""
    in sentencePrefix <> stemMark <> shortMeaning <> caseOrIlloc
  PBias b -> let desc = biasGloss b
             in if T.null desc then T.pack (show b) else "\x201C" <> desc <> "\x201D"
  PRegister r -> T.pack (show r)
  PReferential refs mc _vc ext -> glossReferentials refs mc ""
    <> maybe "" glossRefExt ext
  PModular pairs fv _raw -> T.intercalate "+" (map glossSlotVIII pairs)
                          <> (if T.null fv then "" else "-" <> fv)
  PAffixual cs deg atype _ -> cs <> "/" <> T.pack (show deg)
    <> case atype of { 1 -> "₁"; 2 -> "₂"; 3 -> "₃"; _ -> "" }
  PMultipleAffix first cz moreAfxs mVz ->
    glossOneAffix affixes first <> "-" <> glossCz cz
    <> T.concat (map (\p -> "-" <> glossOneAffix affixes p) moreAfxs)
    <> maybe "" (\vz -> "-" <> glossVz vz) mVz
  PCombinationRef refs mc _spec afxs mc2 ->
    glossCombinationRefs refs
    <> case mc2 of
         Just _ -> maybe "" (\c -> "-" <> T.pack (showCase c)) mc
         Nothing -> ""
    <> T.concat (map (\p -> "-" <> glossOneAffix affixes p) afxs)
    <> maybe "" (\c -> "-" <> T.pack (showCase c)) mc2
  PCarrier _ct content -> content
  PMoodCaseScope ms -> glossMoodOrScope ms
  PError msg _ -> "Error: " <> msg
  PUnparsed t -> "?" <> t

-- | Extract VxCs affix pairs from Ca rest conjuncts
-- The first consonant is Ca itself; subsequent V-C pairs are Slot VII affixes
-- If the last pair's consonant is a Cn consonant, it's VnCn (Slot VIII)
-- For absolute level (V+y+V+Cn), drops the last two pairs
extractAffixes :: [Text] -> [(Text, Text)]  -- (Vx vowel, Cs consonant)
extractAffixes parts =
  let allPairs = extractAllPairs parts
      vnCnFull = extractVnCnFull parts
  in case vnCnFull of
    Just (_, _, True) | length allPairs >= 2 -> take (length allPairs - 2) allPairs
    Just _ -> init allPairs
    Nothing -> allPairs

-- | Extract the VnCn pair from Ca rest conjuncts (if present)
-- Also detects absolute level pattern: V + y + V + Cn → combined Vn with absolute flag
-- Returns (Vn vowel, Cn consonant, is absolute level?)
extractVnCn :: [Text] -> Maybe (Text, Text)
extractVnCn parts = case extractVnCnFull parts of
  Just (vn, cn, _) -> Just (vn, cn)
  Nothing -> Nothing

-- | Full VnCn extraction with absolute level detection
extractVnCnFull :: [Text] -> Maybe (Text, Text, Bool)
extractVnCnFull parts =
  let allPairs = extractAllPairs parts
  in case allPairs of
    [] -> Nothing
    _ | length allPairs >= 2
      , let (vn1, c1) = allPairs !! (length allPairs - 2)
      , c1 == "y"
      , isCnConsonant (snd (last allPairs))
      , let combinedVn = vn1 <> fst (last allPairs)
      , isLevelVowel (normalizeAccents combinedVn)
      -> Just (combinedVn, snd (last allPairs), True)
      | isCnConsonant (snd (last allPairs)) -> Just (fst (last allPairs), snd (last allPairs), False)
      | otherwise -> Nothing

-- | Check if a vowel is a Level (Series 4) vowel
isLevelVowel :: Text -> Bool
isLevelVowel v = isJust (lookup v levelVowels)

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

-- | Case-accessor/case-stacking affix consonants
-- w-series: Vx is case vowel directly
-- y-series: Vx needs glottalization (V → V'V for monophthongs, V1V2 → V1'V2)
isCaseAffix :: Text -> Bool
isCaseAffix cs = cs `elem` (["sw", "zw", "čw", "šw", "žw", "jw", "lw",
                              "sy", "zy", "čy", "šy", "žy", "jy", "ly"] :: [Text])

-- | Get the case-accessor kind abbreviation
caseAffixKind :: Text -> Text
caseAffixKind cs = case cs of
  "sw" -> "acc₁"; "sy" -> "acc₁"; "zw" -> "acc₂"; "zy" -> "acc₂"
  "čw" -> "acc₃"; "čy" -> "acc₃"
  "šw" -> "ia₁";  "šy" -> "ia₁";  "žw" -> "ia₂";  "žy" -> "ia₂"
  "jw" -> "ia₃";  "jy" -> "ia₃"
  "lw" -> "case"; "ly" -> "case"
  _    -> cs

-- | Glottalize a vowel for y-series case affixes
-- Monophthongs: V → V'V (a → a'a), Diphthongs: V1V2 → V1'V2 (ai → a'i)
glottalizeVowel :: Text -> Text
glottalizeVowel v
  | T.length v == 1 = v <> "'" <> v
  | T.length v >= 2 = T.take 1 v <> "'" <> T.drop 1 v
  | otherwise = v

-- | Ca-stacking vowel: when Vx = "üö", Cs is interpreted as Ca complex
caStackingVowel :: Text
caStackingVowel = "üö"

-- | Gloss a single affix (Vx, Cs) pair
glossOneAffix :: Map Text AffixEntry -> (Text, Text) -> Text
glossOneAffix affixes (vx, cs)
  | cs == "nļ" =
    -- IVL affix: Vx encodes Illocution (Series 1) or Illocution+Validation (Series 2)
    let nv = normalizeAccents vx
    in case vowelFormLookup nv of
      Just (1, form) -> case form of
        1 -> "ASR"; 2 -> "DIR"; 3 -> "DEC"; 4 -> "IRG"; 5 -> "VER"
        6 -> "ADM"; 7 -> "POT"; 8 -> "HOR"; 9 -> "CNJ"; _ -> "?" <> vx
      Just (2, form) -> "ASR/" <> case form of
        1 -> "OBS"; 2 -> "REC"; 3 -> "PUP"; 4 -> "RPR"; 5 -> "USP"
        6 -> "IMA"; 7 -> "CVN"; 8 -> "ITU"; 9 -> "INF"; _ -> "?" <> vx
      _ -> "?" <> vx
  | normalizeAccents vx == caStackingVowel =
    let caGloss = case parseCa cs of
          Just pc -> showCaAbbr pc
          Nothing -> cs
    in "Ca:" <> (if T.null caGloss then "default" else caGloss)
  | isCaseAffix cs =
    let -- For y-series, glottalize the vowel to get the case vowel
        caseVowel = if T.isSuffixOf "y" cs
          then glottalizeVowel (normalizeAccents vx)
          else normalizeAccents vx
        caseName = case parseCase caseVowel of
          Just c -> T.pack (showCase c)
          Nothing -> "?" <> vx
    in caseAffixKind cs <> ":" <> caseName
  | otherwise =
    let (degree, atype) = classifyDegreeType vx
        abbr = case lookupAffix cs affixes of
          Just entry -> affixAbbrev entry
          Nothing -> cs
        typeSuffix = case atype of
          1 -> "₁"; 2 -> "₂"; 3 -> "₃"; _ -> ""
    in abbr <> "/" <> T.pack (show degree) <> typeSuffix

-- | Determine affix degree (1-9, 0) and type (1-3) from Vx vowel
-- Returns (degree, type) where type is the series number
classifyDegreeType :: Text -> (Int, Int)
classifyDegreeType v = case vowelFormLookup (normalizeAccents v) of
  Just (series, form) -> (form, series)
  Nothing -> case normalizeAccents v of
    -- Degree-0 forms
    "ae" -> (0, 1); "ea" -> (0, 2); "üo" -> (0, 3)
    _ -> (0, 1)

-- | Determine affix degree (1-9, 0) from Vx vowel
classifyDegree :: Text -> Int
classifyDegree = fst . classifyDegreeType

selectStem :: Stem -> RootEntry -> Text
selectStem S0 = rootStem0
selectStem S1 = rootStem1
selectStem S2 = rootStem2
selectStem S3 = rootStem3

-- | Gloss a SlotVIII (VnCn) value
glossSlotVIII :: SlotVIII -> Text
glossSlotVIII (VnCnValence val ms) = T.pack (show val) <> "-" <> glossMoodOrScope ms
glossSlotVIII (VnCnPhase ph ms) = T.pack (show ph) <> "-" <> glossMoodOrScope ms
glossSlotVIII (VnCnLevel lvl absLvl ms) =
  T.pack (show lvl) <> (if absLvl then ".a" else "") <> "-" <> glossMoodOrScope ms
glossSlotVIII (VnCnEffect eff ms) = T.pack (show eff) <> "-" <> glossMoodOrScope ms
glossSlotVIII (VnCnAspect asp ms) = T.pack (show asp) <> "-" <> glossMoodOrScope ms

glossMoodOrScope :: MoodOrScope -> Text
glossMoodOrScope (MoodVal m) = T.pack (show m)
glossMoodOrScope (CaseScope cs) = T.pack (show cs)

showCaAbbr :: ParsedCa -> Text
showCaAbbr pc
  | pc == ParsedCa UNI CSL M_ DEL NRM = ""  -- Default, don't show
  | otherwise = T.intercalate "." $ filter (/= "") $
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

-- | Gloss an extended referential suffix (w/y scope + second case/referent)
glossRefExt :: (Text, Maybe Case, Maybe PersonalRef) -> Text
glossRefExt (wy, mc2, mRef2) =
  let scope = if wy == "w" then "\\RPV" else ""
      case2 = maybe "" (\c -> "-" <> T.pack (showCase c)) mc2
      ref2 = maybe "" (\pr -> "-" <> glossOneRef pr) mRef2
  in scope <> case2 <> ref2

-- | Gloss a single referent
glossOneRef :: PersonalRef -> Text
glossOneRef (PersonalRef ref eff) =
  let label = referentAbbrev ref
      effAbbr = case eff of
        NEU -> ""
        BEN -> ".BEN"
        DET -> ".DET"
  in label <> effAbbr

-- | Gloss a referential word (possibly multiple referents from cluster)
glossReferentials :: [PersonalRef] -> Maybe Case -> Text -> Text
glossReferentials refs mc _vc =
  let refPart = case refs of
        [] -> "?"
        rs -> T.intercalate "+" (map glossOneRef rs)
      caseAbbr = case mc of
        Just c -> "-" <> T.pack (showCase c)
        Nothing -> ""
  in refPart <> caseAbbr

-- | Gloss a combination referential
-- Uses bracket notation only for multi-referent clusters
glossCombinationRefs :: [PersonalRef] -> Text
glossCombinationRefs refs = case refs of
  [] -> "?"
  [r] -> glossOneRef r
  rs -> "[" <> T.intercalate "+" (map glossOneRef rs) <> "]"

--------------------------------------------------------------------------------
-- Multi-word Sentence Glossing
--------------------------------------------------------------------------------

-- | Parse and gloss a complete sentence (space-separated words)
-- Uses context-aware glossing: carrier adjuncts cause following words to be
-- treated as foreign/quoted text until a terminator (hü) or sentence boundary
glossSentence :: Map Text RootEntry -> Map Text AffixEntry -> Text -> Text
glossSentence roots affixes sentence =
  let pairs = glossSentenceWords roots affixes sentence
  in T.intercalate "  " $ filter (not . T.null) $ map snd pairs

-- | Context state for sentence-level glossing
data GlossContext = GlossContext
  { gcFollowsCarrier :: Bool   -- ^ Next word follows a carrier (treated as foreign)
  , gcTerminated     :: Bool   -- ^ A terminator exists ahead; after consuming foreign word, check for it
  } deriving (Show)

-- | Parse and gloss a sentence, returning (original word, gloss) pairs
-- Handles carrier adjunct scoping: words after a carrier are glossed as foreign text
glossSentenceWords :: Map Text RootEntry -> Map Text AffixEntry -> Text -> [(Text, Text)]
glossSentenceWords roots affixes sentence =
  go (GlossContext False False) (T.words sentence)
  where
    stripPunct w = T.filter (\c -> c /= ',' && c /= '.' && c /= '!' && c /= '?' && c /= ':' && c /= ';') w
    go _ [] = []
    go ctx (w:rest) =
      let clean = stripPunct w
      in if T.null clean then go ctx rest
         else if "*" `T.isPrefixOf` clean && "*" `T.isSuffixOf` clean
           -- Quoted text in asterisks (names/foreign words)
           then (w, T.drop 1 (T.dropEnd 1 clean)) : go ctx rest
         else if gcFollowsCarrier ctx
           -- Word follows a carrier: treat as foreign text, then reset carrier flag
           then let isEnd = isTerminator clean
                    ctx' = ctx { gcFollowsCarrier = False
                               , gcTerminated = if isEnd then False else gcTerminated ctx }
                in (w, clean) : go ctx' rest
         else if gcTerminated ctx && isTerminator clean
           -- Terminator word (hü) ending a carrier phrase
           then let parsed = parseWord clean
                    gloss = glossWord roots affixes parsed
                in (w, gloss) : go (ctx { gcTerminated = False }) rest
         else
           let parsed = parseWord clean
               -- Check if this word is a carrier
               carrier = isCarrierParsed parsed
               -- Look ahead for terminator within this sentence
               hasTerminator = carrier && any (isTerminator . stripPunct) (takeWhile (not . isSentenceEnd) rest)
               ctx' = if carrier
                      then GlossContext True hasTerminator
                      else ctx
               gloss = glossWord roots affixes parsed
           in (w, gloss) : go ctx' rest

-- | Check if a parsed word is a carrier (adjunct or formative with root -s-)
isCarrierParsed :: ParsedWord -> Bool
isCarrierParsed (PCarrier _ _) = True
isCarrierParsed (PFormative pf) = pfRoot pf == Root "s"
isCarrierParsed (PConcatenated pfs) = any (\pf -> pfRoot pf == Root "s") pfs
isCarrierParsed (PReferential _ _ _ _) = False  -- referential carriers use hl/hn/hň which parse as PCarrier
isCarrierParsed _ = False

-- | Check if a word is a carrier/register terminator (hü = END register)
isTerminator :: Text -> Bool
isTerminator w = T.toLower w == "hü"

-- | Check if a word ends a sentence (punctuation)
isSentenceEnd :: Text -> Bool
isSentenceEnd w = any (`T.isSuffixOf` w) [".", "!", "?"]

-- | Invalid root consonant forms (per Ithkuil V4 spec)
invalidRootForms :: [Text]
invalidRootForms = ["ļ", "ç", "çç", "çw", "w", "y"]

-- | Validate all formative constraints
-- Returns Just error if invalid
validateFormative :: ParsedFormative -> Maybe Text
validateFormative pf =
  let Root cr = pfRoot pf
  in if cr `elem` invalidRootForms
     then Just ("Invalid root form: " <> cr)
     else validateSlotVMarker pf

-- | Validate Slot V marker consistency
-- Returns Nothing if valid, Just error message if mismatch
validateSlotVMarker :: ParsedFormative -> Maybe Text
validateSlotVMarker pf
  | pfHasShortcut pf =
      -- Shortcuts: glottal at Vv means slot V is filled, requiring >=2 VxCs affixes
      let affixCount = length (extractAffixes (pfCa pf))
      in if pfSlotVMarker pf && affixCount < 2
         then Just "Unexpectedly few slot V affixes"
         else if not (pfSlotVMarker pf) && affixCount >= 2
         then Just "Unexpectedly many slot V affixes"
         else Nothing
  | pfSlotVMarker pf && length (pfSlotV pf) < 2 =
      Just "Unexpectedly few slot V affixes"
  | not (pfSlotVMarker pf) && length (pfSlotV pf) >= 2 =
      Just "Unexpectedly many slot V affixes"
  | otherwise = Nothing
