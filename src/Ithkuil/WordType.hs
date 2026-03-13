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
  , glossCz
  , glossVz
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import Ithkuil.Grammar
import Data.Maybe (isJust)
import Ithkuil.Phonology (vowelFormLookup)
import Ithkuil.Parse (splitConjuncts, isVowelChar, parseCase, ParsedFormative(..), parseFormativeReal, ParsedCa(..), isSpecialVv, normalizeAccents, detectStressSimple)
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
  | PReferential PersonalRef (Maybe Case) Text (Maybe (Text, Maybe Case, Maybe PersonalRef))
    -- ^ referent, case, raw case vowel, optional (scope w/y, case2, ref2)
  | PAffixual Text Int Text     -- affix Cs, degree, optional scope vowel
  | PMultipleAffix (Text, Text) Text [(Text, Text)] (Maybe Text)
    -- ^ first affix (Vx,Cs), Cz scope, additional affixes (Vx,Cs), optional Vz scope
  | PCarrier CarrierType Text   -- carrier type, content
  | PCombinationRef PersonalRef (Maybe Case) Text [(Text, Text)] (Maybe Case)
    -- ^ referent, case1, spec, affixes (VxCs), case2
  | PMoodCaseScope MoodOrScope  -- standalone mood/case-scope adjunct
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
-- C1 must be a referential consonant, w/y marks RPV/perspective scope
isReferentialWord :: Text -> Bool
isReferentialWord word =
  let conjs = splitConjuncts word
      -- Strip optional ë/äi prefix
      rest0 = case conjs of
        ("ë":cs)  -> cs
        ("äi":cs) -> cs
        cs        -> cs
      -- Consume referential consonants (C1 [ë C1]*)
      consumeRefs (c:cs)
        | isRefC1 c = case cs of
            ("ë":c2:cs2) | isRefC1 c2 -> consumeRefs (c2:cs2)
            _ -> Just cs
        | otherwise = Nothing
      consumeRefs [] = Nothing
      -- After C1s, expect: V [w/y V [C1 [ë]]]
      isValidTail cs = case cs of
        [v]            | isV v -> True
        [v, wy, v2]   | isV v && wy `elem` ["w", "y"] && isV v2 -> True
        [v, wy, v2, c] | isV v && wy `elem` ["w", "y"] && isV v2 && isRefC1 c -> True
        _ -> False
      isV t = not (T.null t) && isVowelChar (T.head t)
  in case consumeRefs rest0 of
    Just tail' -> isValidTail tail'
    Nothing -> False

-- | Check if a consonant is a valid referential C1
isRefC1 :: Text -> Bool
isRefC1 c = c `elem` map snd refC1All

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
-- No geminate Ca in the consonant slots
isCombinationRef :: Text -> Bool
isCombinationRef word =
  let conjs = splitConjuncts word
      -- Strip optional ë prefix
      rest0 = case conjs of
        ("ë":cs) -> cs
        cs       -> cs
      specConsonants = ["x", "xt", "xp", "xx"] :: [Text]
  in case rest0 of
    (c:v:spec:_) | isRefC1 c
                 , not (T.null v) && isVowelChar (T.head v)
                 , spec `elem` specConsonants -> True
    -- "a" + CP consonant form
    ("a":cp:v:spec:_) | cp `elem` ["hl", "hm", "hn", "hň"]
                      , not (T.null v) && isVowelChar (T.head v)
                      , spec `elem` specConsonants -> True
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
  WFormative -> case parseFormativeReal word of
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

-- | Parse a referential word
-- Simple: C1-Vc, Dual: C1-ë-C1-Vc, Extended: C1-Vc-w/y-Vc2
parseReferentialWord :: Text -> ParsedWord
parseReferentialWord word =
  let conjs = splitConjuncts word
      -- Strip optional ë/äi prefix
      rest0 = case conjs of
        ("ë":cs)  -> cs
        ("äi":cs) -> cs
        cs        -> cs
      -- Consume C1 referential consonants, collecting refs
      consumeRefs (c:cs)
        | isRefC1 c = case lookupRefC1 c of
            Just ref -> case cs of
              ("ë":c2:cs2) -> case consumeRefs (c2:cs2) of
                (refs, tail') -> (ref:refs, tail')
              _ -> ([ref], cs)
            Nothing -> ([], c:cs)
        | otherwise = ([], c:cs)
      consumeRefs [] = ([], [])
      (refs, tail') = consumeRefs rest0
  in case (refs, tail') of
    (ref:_, [v]) ->
      PReferential ref (parseCase v) v Nothing
    (ref:_, [v, wy, v2]) | wy `elem` ["w", "y"] ->
      -- w = RPV effect on second case, y = perspective scope
      PReferential ref (parseCase v) v (Just (wy, parseCase v2, Nothing))
    (ref:_, [v, wy, v2, c2]) | wy `elem` ["w", "y"], isRefC1 c2 ->
      -- Extended: C1-Vc-w/y-Vc2-C2 (dual referential with scope)
      PReferential ref (parseCase v) v (Just (wy, parseCase v2, lookupRefC1 c2))
    _ -> PUnparsed word

-- | Parse an affixual adjunct word (Vx-Cs or Vx-Cs-Vs)
parseAffixualWord :: Text -> ParsedWord
parseAffixualWord word =
  let conjs = splitConjuncts word
  in case conjs of
    [vx, cs] -> PAffixual cs (classifyDegree vx) ""
    [vx, cs, vs] -> PAffixual cs (classifyDegree vx) (glossVz vs)
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
      let ref = lookupRefC1 c1
          case1 = parseCase vc
          -- Parse VxCs affix pairs from rest
          (affixPairs, lastV) = parseVxCsPairs rest
          case2 = lastV >>= parseCase
      in case ref of
        Just r -> PCombinationRef r case1 spec affixPairs case2
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
  PModular pairs fv _ -> "MOD:" <> T.intercalate "+" (map glossSlotVIII pairs)
                       <> (if T.null fv then "" else "-" <> fv)
  PReferential ref mc vc ext -> glossReferential ref mc vc
    <> maybe "" glossRefExt ext
  PAffixual cs deg scope ->
    let abbr = case lookupAffix cs affixes of
          Just entry -> affixAbbrev entry
          Nothing -> cs
    in abbr <> "/" <> T.pack (show deg)
       <> (if T.null scope then "" else "-" <> scope)
  PMultipleAffix first cz moreAfxs mVz ->
    glossOneAffix affixes first <> "-" <> glossCz cz
    <> T.concat (map (\p -> "-" <> glossOneAffix affixes p) moreAfxs)
    <> maybe "" (\vz -> "-" <> glossVz vz) mVz
  PCombinationRef ref mc spec afxs mc2 ->
    glossReferential ref mc ""
    <> (if spec /= "x" then "-" <> spec else "")
    <> T.concat (map (\p -> "-" <> glossOneAffix affixes p) afxs)
    <> maybe "" (\c -> "-" <> T.pack (showCase c)) mc2
  PCarrier ct _ -> "CARRIER:" <> T.pack (show ct)
  PMoodCaseScope ms -> glossMoodOrScope ms
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
        -- Truncate long meanings for compact display
        shortMeaning = if T.length rootMeaning > 25
          then T.take 22 rootMeaning <> "..."
          else rootMeaning
    in stemMark <> shortMeaning <> caseOrIlloc
  PBias b -> T.pack (show b)
  PRegister r -> T.pack (show r)
  PReferential ref mc _vc ext -> glossReferential ref mc ""
    <> maybe "" glossRefExt ext
  PModular pairs fv _raw -> T.intercalate "+" (map glossSlotVIII pairs)
                          <> (if T.null fv then "" else "-" <> fv)
  PAffixual cs deg _ -> cs <> "/" <> T.pack (show deg)
  PMultipleAffix first cz moreAfxs mVz ->
    glossOneAffix _affixes first <> "-" <> glossCz cz
    <> T.concat (map (\p -> "-" <> glossOneAffix _affixes p) moreAfxs)
    <> maybe "" (\vz -> "-" <> glossVz vz) mVz
  PCombinationRef ref mc _spec afxs mc2 ->
    glossReferential ref mc "" <> T.concat (map (\p -> "-" <> glossOneAffix _affixes p) afxs)
    <> maybe "" (\c -> "." <> T.pack (showCase c)) mc2
  PCarrier _ct content -> content
  PMoodCaseScope ms -> glossMoodOrScope ms
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
  let (degree, atype) = classifyDegreeType vx
      abbr = case lookupAffix cs affixes of
        Just entry -> affixAbbrev entry
        Nothing -> cs
      typeSuffix = case atype of
        1 -> ""; 2 -> "₂"; 3 -> "₃"; _ -> ""
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

-- | Gloss an extended referential suffix (w/y scope + second case/referent)
glossRefExt :: (Text, Maybe Case, Maybe PersonalRef) -> Text
glossRefExt (wy, mc2, mRef2) =
  let scope = if wy == "w" then "\\RPV" else ""
      case2 = maybe "" (\c -> "-" <> T.pack (showCase c)) mc2
      ref2 = maybe "" (\(PersonalRef r _) -> "-" <> referentLabel r) mRef2
  in scope <> case2 <> ref2

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
