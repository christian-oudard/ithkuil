{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Composition Helpers
-- Reverse grammar lookups: abbreviation → phonological form
module Ithkuil.Compose
  ( lookupGrammar
  , searchGrammar
  , lookupForm
  , searchRoots
  , searchRootsRanked
  , buildKeywordIndex
  , KeywordIndex
  , searchAffixes
  , allCases
  , allValences
  , allAspects
  , allPhases
  , allLevels
  , allEffects
  , allMoods
  , allIllocutions
  , allValidations
  , allConfigurations
  , allAffiliations
  , allPerspectives
  , allExtensions
  , allEssences
  , allFunctions
  , allSpecifications
  , allContexts
  , allStems
  , allVersions
  , allCaseScopes
  , GrammarEntry(..)
  , dumpGrammarTable
  , composeFormative
  , composeReferential
  , applyStress
  ) where

import Data.List (sortBy)
import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text.Stemming.English (stem)
import Ithkuil.Grammar
import Ithkuil.Render
import Ithkuil.Allomorph (constructCa)
import Ithkuil.Phonology (vowelForm)
import Ithkuil.Lexicon (RootEntry(..), AffixEntry(..), rootStem0, rootStem1, rootStem2, rootStem3)
import Ithkuil.Referentials (PersonalRef(..), refC1)

data GrammarEntry = GrammarEntry
  { gCategory :: Text    -- e.g. "Case", "Aspect"
  , gAbbrev   :: Text    -- e.g. "THM", "RTR"
  , gName     :: Text    -- e.g. "Thematic", "Retrospective"
  , gForm     :: Text    -- e.g. "a", "a" (vowel/consonant form)
  } deriving (Show, Eq)

-- | Look up a grammar abbreviation (exact match)
lookupGrammar :: Text -> [GrammarEntry]
lookupGrammar query =
  let q = T.toUpper query
  in filter (\e -> gAbbrev e == q) grammarTable

-- | Search grammar by abbreviation or name (fuzzy)
searchGrammar :: Text -> [GrammarEntry]
searchGrammar query =
  let q = T.toCaseFold query
      exact = filter (\e -> T.toCaseFold (gAbbrev e) == q) grammarTable
      byName = filter (\e -> q `T.isInfixOf` T.toCaseFold (gName e)) grammarTable
      byCat  = filter (\e -> q `T.isInfixOf` T.toCaseFold (gCategory e)) grammarTable
  in if not (null exact) then exact
     else if not (null byName) then byName
     else byCat

-- | Reverse form lookup: given a vowel or consonant form, show all grammar
-- values it could represent. E.g. "a" → THM case, RTR aspect, MNO valence...
lookupForm :: Text -> [GrammarEntry]
lookupForm form = filter (\e -> gForm e == form) grammarTable

-- | Search roots by keyword in any stem meaning (substring match, unranked)
searchRoots :: Text -> Map Text RootEntry -> [(Text, RootEntry)]
searchRoots query roots =
  let q = T.toCaseFold query
      matches (cr, entry) =
        let stems = [rootStem0 entry, rootStem1 entry, rootStem2 entry, rootStem3 entry]
        in any (T.isInfixOf q . T.toCaseFold) stems
           || T.toCaseFold cr == q
  in filter matches (Map.toList roots)

-- | Search roots ranked by relevance (lower score = better match)
-- Supports both English keywords and consonant root forms (e.g. "jl" or "-jl-")
-- Uses Porter2 stemming via keyword index for morphological matching.
searchRootsRanked :: Text -> Map Text RootEntry -> [(Int, Text, RootEntry)]
searchRootsRanked query roots =
  let q = T.toCaseFold query
      stripped = T.dropWhile (== '-') . T.dropWhileEnd (== '-') $ q
      directHit = case Map.lookup stripped roots of
        Just entry -> [(0, stripped, entry)]
        Nothing    -> []
  in if not (null directHit) then directHit
     else let idx = buildKeywordIndex roots
              hits = lookupKeywordIndex q idx
              resolved = [ (score, cr, entry)
                         | (score, cr) <- hits
                         , Just entry <- [Map.lookup cr roots] ]
          in sortBy (compare `on` (\(s,cr,_) -> (s, T.length cr, cr))) resolved

-- | Inverted keyword index: stemmed word → [(score, Cr root)]
-- Score encodes match quality: lower = fragment is more central to root meaning.
type KeywordIndex = Map Text [(Int, Text)]

-- | Build keyword index from roots lexicon using Porter2 stemming.
-- Bag-of-words approach: each stem description is tokenized, stemmed, and indexed.
-- Score = word count of the shortest description containing this word.
buildKeywordIndex :: Map Text RootEntry -> KeywordIndex
buildKeywordIndex roots =
  Map.fromListWith (++) $ concatMap indexRoot (Map.toList roots)
  where
    indexRoot (cr, entry) =
      let descs = [ (0, T.toCaseFold (rootStem1 entry))  -- S1 primary
                  , (1, T.toCaseFold (rootStem2 entry))  -- S2 secondary
                  , (1, T.toCaseFold (rootStem3 entry))  -- S3 tertiary
                  , (2, T.toCaseFold (rootStem0 entry))  -- S0 generic
                  ]
          -- S0 single-word slash-parts get synonym boost (score 1)
          s0parts = map (T.strip . T.toCaseFold) . T.splitOn "/" $
                    rootStem0 entry
          s0synonyms = filter (\p -> not (T.any (== ' ') p) && T.length p > 1)
                       s0parts
          synEntries = concatMap (\syn ->
            let forms = Set.toList . Set.fromList $ [syn, porterStem syn]
            in [(w, [(5, cr)]) | w <- forms]  -- S0 synonyms rank below direct S1 matches
            ) s0synonyms
      in synEntries ++ concatMap (\(pri, d) -> indexDesc cr pri d) descs

    indexDesc cr pri desc =
      let stripped = stripParens desc
          ws = extractWords stripped
          nWords = length ws
          baseScore = pri * 10 + nWords
          -- Exact-match bonus: if description is a single word, give it a strong boost
          trimmed = T.strip (T.toCaseFold stripped)
          exactBonus w = if nWords == 1 && (w == trimmed || porterStem w == porterStem trimmed)
                         then 0  -- override to perfect score
                         else baseScore
          mainForms = Set.toList . Set.fromList $ ws ++ map porterStem ws
          -- Parenthetical-only words score at full word count + 1
          allWs = extractWords desc
          nFull = length allWs
          parenScore = pri * 10 + nFull + 1
          parenOnly = Set.toList $ Set.fromList (allWs ++ map porterStem allWs)
                      `Set.difference` Set.fromList mainForms
      in [(w, [(exactBonus w, cr)]) | w <- mainForms]
         ++ [(w, [(parenScore, cr)]) | w <- parenOnly]

    -- | Remove parenthetical content for word counting (taxonomy names inflate scores)
    stripParens :: Text -> Text
    stripParens = T.concat . go 0 . T.unpack
      where
        go _ [] = []
        go n ('(':cs) = go (n + 1 :: Int) cs
        go n (')':cs) = go (max 0 (n - 1)) cs
        go 0 (c:cs)   = [T.singleton c] ++ go 0 cs
        go n (_:cs)   = go n cs

    extractWords :: Text -> [Text]
    extractWords t =
      filter (\w -> T.length w > 1) .
      map (T.dropWhile (\c -> not (c >= 'a' && c <= 'z'))) .
      filter (T.any (\c -> c >= 'a' && c <= 'z')) .
      T.words $ T.map (\c -> if c `elem` (".,;:!?()-/''\x2019" :: String) then ' ' else c) t

    porterStem = stem Set.empty

-- | Look up a query in the keyword index, trying exact word then stem.
lookupKeywordIndex :: Text -> KeywordIndex -> [(Int, Text)]
lookupKeywordIndex q idx =
  let exactHits = Map.findWithDefault [] q idx
      stemmed = stem Set.empty q
      stemHits = if stemmed /= q
                 then Map.findWithDefault [] stemmed idx
                 else []
      allHits = exactHits ++ stemHits
      -- Deduplicate: keep best score per root
      deduped = Map.toList $ Map.fromListWith min [(cr, score) | (score, cr) <- allHits]
  in [(score, cr) | (cr, score) <- deduped]

-- | Search affixes by keyword in abbreviation, description, or degree meanings
-- Also handles -Cs- notation (strips dashes for consonant form lookup)
searchAffixes :: Text -> Map Text AffixEntry -> [(Text, AffixEntry)]
searchAffixes query affixes =
  let q = T.toCaseFold query
      stripped = T.dropWhile (== '-') . T.dropWhileEnd (== '-') $ q
      matches (cs, entry) =
        T.toCaseFold cs == stripped
        || T.toCaseFold (affixAbbrev entry) == T.toUpper q
        || T.isInfixOf q (T.toCaseFold (affixDesc entry))
        || any (T.isInfixOf q . T.toCaseFold) (affixDegrees entry)
  in filter matches (Map.toList affixes)

-- | Complete grammar table for reverse lookup
grammarTable :: [GrammarEntry]
grammarTable = concat
  [ allCases, allValences, allAspects, allPhases, allLevels, allEffects
  , allMoods, allIllocutions, allValidations, allCaseScopes
  , allConfigurations, allAffiliations, allPerspectives, allExtensions, allEssences
  , allFunctions, allSpecifications, allContexts, allStems, allVersions
  ]

-- Cases (68)
allCases :: [GrammarEntry]
allCases = concat
  [ map (\c -> ge "Case/Trans" (sn c) (nm c) (renderCase (Transrelative c))) [minBound..maxBound]
  , map (\c -> ge "Case/Appos" (sn c) (nm c) (renderCase (Appositive c))) [minBound..maxBound]
  , map (\c -> ge "Case/Assoc" (sn c) (nm c) (renderCase (Associative c))) [minBound..maxBound]
  , map (\c -> ge "Case/Advrb" (sn c) (nm c) (renderCase (Adverbial c))) [minBound..maxBound]
  , map (\c -> ge "Case/Relat" (sn c) (nm c) (renderCase (Relational c))) [minBound..maxBound]
  , map (\c -> ge "Case/Affin" (sn c) (nm c) (renderCase (Affinitive c))) [minBound..maxBound]
  , map (\c -> ge "Case/SptTm1" (sn c) (nm c) (renderCase (SpatioTemporal1 c))) [minBound..maxBound]
  , map (\c -> ge "Case/SptTm2" (sn c) (nm c) (renderCase (SpatioTemporal2 c))) [minBound..maxBound]
  ]

allValences :: [GrammarEntry]
allValences = map (\v -> ge "Valence" (sn v) (nm v) (renderValence v)) [minBound..maxBound]

allAspects :: [GrammarEntry]
allAspects = map (\a -> ge "Aspect" (sn a) (nm a) (renderAspect a)) [minBound..maxBound]

allPhases :: [GrammarEntry]
allPhases = map (\p -> ge "Phase" (sn p) (nm p) (renderPhase p)) [minBound..maxBound]

allLevels :: [GrammarEntry]
allLevels = map (\l -> ge "Level" (sn l) (nm l) (renderLevel l)) [minBound..maxBound]

allEffects :: [GrammarEntry]
allEffects = map (\e -> ge "Effect" (sn e) (nm e) (renderEffect e)) [minBound..maxBound]

allMoods :: [GrammarEntry]
allMoods = map (\m -> ge "Mood" (sn m) (nm m) (renderMoodOrScope (MoodVal m))) [minBound..maxBound]

allIllocutions :: [GrammarEntry]
allIllocutions = map (\i -> ge "Illocution" (sn i) (nm i) (renderIllocution i)) [minBound..maxBound]

allValidations :: [GrammarEntry]
allValidations = map (\v -> ge "Validation" (sn v) (nm v) (renderValidation v)) [minBound..maxBound]

allCaseScopes :: [GrammarEntry]
allCaseScopes = map (\cs -> ge "CaseScope" (sn cs) (nm cs) (renderMoodOrScope (CaseScope cs))) [minBound..maxBound]

allConfigurations :: [GrammarEntry]
allConfigurations = map (\c -> ge "Configuration" (sn c) (nm c) (renderConfiguration c)) [minBound..maxBound]

allAffiliations :: [GrammarEntry]
allAffiliations = map (\a -> ge "Affiliation" (sn a) (nm a) (renderAffiliation a)) [minBound..maxBound]

allPerspectives :: [GrammarEntry]
allPerspectives =
  [ ge "Perspective" "M" "Monadic" "l"
  , ge "Perspective" "G" "Polyadic" "r"
  , ge "Perspective" "N" "Nomic" "w"
  , ge "Perspective" "A" "Abstract" "y"
  ]

allExtensions :: [GrammarEntry]
allExtensions = map (\e -> ge "Extension" (sn e) (nm e) (renderExtension e)) [minBound..maxBound]

allEssences :: [GrammarEntry]
allEssences =
  [ ge "Essence" "NRM" "Normal" "(default)"
  , ge "Essence" "RPV" "Representative" "ř→l, l→m, etc."
  ]

allFunctions :: [GrammarEntry]
allFunctions =
  [ ge "Function" "STA" "Stative" "Vr series 1-4 form 1-4"
  , ge "Function" "DYN" "Dynamic" "Vr series 1-4 form 9-6"
  ]

allSpecifications :: [GrammarEntry]
allSpecifications =
  [ ge "Specification" "BSC" "Basic" "form 1/9"
  , ge "Specification" "CTE" "Contential" "form 2/8"
  , ge "Specification" "CSV" "Constitutive" "form 3/7"
  , ge "Specification" "OBJ" "Objective" "form 4/6"
  ]

allContexts :: [GrammarEntry]
allContexts =
  [ ge "Context" "EXS" "Existential" "series 1"
  , ge "Context" "FNC" "Functional" "series 2"
  , ge "Context" "RPS" "Representational" "series 3"
  , ge "Context" "AMG" "Amalgamative" "series 4"
  ]

allStems :: [GrammarEntry]
allStems =
  [ ge "Stem" "S1" "Stem 1" "a/ä (PRC/CPT)"
  , ge "Stem" "S2" "Stem 2" "e/i (PRC/CPT)"
  , ge "Stem" "S3" "Stem 3" "u/ü (PRC/CPT)"
  , ge "Stem" "S0" "Stem 0" "o/ö (PRC/CPT)"
  ]

allVersions :: [GrammarEntry]
allVersions =
  [ ge "Version" "PRC" "Processual" "first vowel of pair"
  , ge "Version" "CPT" "Completive" "second vowel of pair"
  ]

-- Helpers
ge :: Text -> Text -> Text -> Text -> GrammarEntry
ge = GrammarEntry

sn :: Show a => a -> Text
sn = T.pack . show

nm :: Show a => a -> Text
nm x = case T.pack (show x) of
  "THM" -> "Thematic"; "INS" -> "Instrumental"; "ABS" -> "Absolutive"
  "AFF" -> "Affective"; "STM" -> "Stimulative"; "EFF" -> "Effectuative"
  "ERG" -> "Ergative"; "DAT" -> "Dative"; "IND" -> "Inducive"
  "POS" -> "Possessive"; "PRP" -> "Proprietive"; "GEN" -> "Genitive"
  "ATT" -> "Attributive"; "PDC" -> "Productive"; "ITP" -> "Interpretive"
  "OGN" -> "Originative"; "IDP" -> "Interdependent"; "PAR" -> "Partitive"
  "APL" -> "Applicative"; "PUR" -> "Purposive"; "TRA" -> "Transmissive"
  "DFR" -> "Deferential"; "CRS" -> "Contrastive"; "TSP" -> "Transpositive"
  "CMM" -> "Commutative"; "CMP" -> "Comparative"; "CSD" -> "Considerative"
  "FUN" -> "Functive"; "TFM" -> "Transformative"; "CLA" -> "Classificative"
  "RSL" -> "Resultative"; "CSM" -> "Consumptive"; "CON" -> "Concessive"
  "AVR" -> "Aversive"; "CVS" -> "Conversive"; "SIT" -> "Situative"
  "PRN" -> "Pertinential"; "DSP" -> "Descriptive"; "COR" -> "Correlative"
  "CPS" -> "Compositive"; "COM" -> "Comitative"; "UTL" -> "Utilitative"
  "PRD" -> "Predicative"; "RLT" -> "Relative"
  "ACT" -> "Activative"; "ASI" -> "Assimilative"; "ESS" -> "Essive"
  "TRM" -> "Terminative"; "SEL" -> "Selective"; "CFM" -> "Conformative"
  "DEP" -> "Dependent"; "VOC" -> "Vocative"
  "LOC" -> "Locative"; "ATD" -> "Attendant"; "ALL" -> "Allative"
  "ABL" -> "Ablative"; "ORI" -> "Orientative"; "IRL" -> "Interrelative"
  "INV" -> "Intrative"; "NAV" -> "Navigative"
  "CNR" -> "Concursive"; "ASS" -> "Assessive"; "PER" -> "Periodic"
  "PRO" -> "Prolapsive"; "PCV" -> "Precursive"; "PCR" -> "Postcursive"
  "ELP" -> "Elapsive"; "PLM" -> "Prolimitive"
  -- Valence
  "MNO" -> "Monoactive"; "PRL" -> "Parallel"; "CRO" -> "Corollary"
  "RCP" -> "Reciprocal"; "CPL" -> "Complementary"; "DUP" -> "Duplicative"
  "DEM" -> "Demonstrative"; "CNG" -> "Contingent"; "PTI" -> "Participative"
  -- Phase
  "PUN" -> "Punctual"; "ITR" -> "Iterative"; "REP" -> "Repetitive"
  "ITM" -> "Intermittent"; "RCT" -> "Recurrent"; "FRE" -> "Frequentative"
  "FRG" -> "Fragmentative"; "VAC" -> "Vacillative"; "FLC" -> "Fluctuative"
  -- Aspect
  "RTR" -> "Retrospective"; "PRS" -> "Prospective"; "HAB" -> "Habitual"
  "PRG" -> "Progressive"; "IMM" -> "Imminent"; "PCS" -> "Precessive"
  "REG" -> "Regressive"; "SMM" -> "Summative"; "ATP" -> "Anticipatory"
  "RSM" -> "Resumptive"; "CSS" -> "Cessative"; "PAU" -> "Pausal"
  "RGR" -> "Regressive2"; "PCL" -> "Preclusive"; "CNT" -> "Continuative"
  "ICS" -> "Incessative"; "EXP" -> "Experiential"; "IRP" -> "Interruptive"
  "PMP" -> "Preemptive"; "CLM" -> "Climactic"; "DLT" -> "Dilatory"
  "TMP" -> "Temporary"; "XPD" -> "Expeditive"; "LIM" -> "Limitative"
  "EPD" -> "Expeditious"; "PTC" -> "Protractive"; "PPR" -> "Preparatory"
  "DCL" -> "Disclusive"; "CCL" -> "Conclusive"; "CUL" -> "Culminative"
  "IMD" -> "Intermediative"; "TRD" -> "Tardative"; "TNS" -> "Transitional"
  "ITC" -> "Intercommutative"; "MTV" -> "Motive"; "SQN" -> "Sequential"
  -- Level
  "MIN" -> "Minimal"; "SBE" -> "Subequative"; "IFR" -> "Inferior"
  "DFT" -> "Deficient"; "EQU" -> "Equative"; "SUR" -> "Surpassive"
  "SPL" -> "Superlative"; "SPQ" -> "Superequative"; "MAX" -> "Maximal"
  -- Effect
  "BEN1" -> "Beneficial/speaker"; "BEN2" -> "Beneficial/addressee"
  "BEN3" -> "Beneficial/3rd"; "BSLF" -> "Beneficial/self"
  "UNK" -> "Unknown"; "DSLF" -> "Detrimental/self"
  "DET3" -> "Detrimental/3rd"; "DET2" -> "Detrimental/addressee"
  "DET1" -> "Detrimental/speaker"
  -- Mood
  "FAC" -> "Factual"; "SUB" -> "Subjunctive"; "ASM" -> "Assumptive"
  "SPC" -> "Speculative"; "COU" -> "Counterfactive"; "HYP" -> "Hypothetical"
  -- Illocution
  "ASR" -> "Assertive"; "DIR" -> "Directive"; "DEC" -> "Declarative"
  "IRG" -> "Interrogative"; "VER" -> "Verificative"; "ADM" -> "Admonitive"
  "POT" -> "Potentiative"; "HOR" -> "Hortative"; "CNJ" -> "Conjectural"
  -- Validation
  "OBS" -> "Observational"; "REC" -> "Recollective"; "PUP" -> "Purportive"
  "RPR" -> "Reportive"; "USP" -> "Unspecified"; "IMA" -> "Imaginary"
  "CVN" -> "Conventional"; "ITU" -> "Intuitive"; "INF" -> "Inferential"
  -- CaseScope
  "CCN" -> "Natural"; "CCA" -> "Antecedent"; "CCS" -> "Subaltern"
  "CCQ" -> "Qualifier"; "CCP" -> "Precedent"; "CCV" -> "Successive"
  -- Configuration
  "UNI" -> "Uniplex"; "DPX" -> "Duplex"
  "DSS" -> "Duplex/Sim/Sep"; "DSC" -> "Duplex/Sim/Con"
  "DSF" -> "Duplex/Sim/Fus"; "DDS" -> "Duplex/Dis/Sep"
  "DDC" -> "Duplex/Dis/Con"; "DDF" -> "Duplex/Dis/Fus"
  "DFS" -> "Duplex/Fuz/Sep"; "DFC" -> "Duplex/Fuz/Con"
  "DFF" -> "Duplex/Fuz/Fus"
  "MSS" -> "Multi/Sim/Sep"; "MSC" -> "Multi/Sim/Con"
  "MSF" -> "Multi/Sim/Fus"; "MDS" -> "Multi/Dis/Sep"
  "MDC" -> "Multi/Dis/Con"; "MDF" -> "Multi/Dis/Fus"
  "MFS" -> "Multi/Fuz/Sep"; "MFC" -> "Multi/Fuz/Con"
  "MFF" -> "Multi/Fuz/Fus"
  -- Affiliation
  "CSL" -> "Consolidative"; "ASO" -> "Associative"
  "COA" -> "Coalescent"; "VAR" -> "Variative"
  -- Extension
  "DEL" -> "Delimitive"; "PRX" -> "Proximal"; "ICP" -> "Incipient"
  "ATV" -> "Attenuative"; "GRA" -> "Graduative"; "DPL" -> "Depletive"
  -- Essence
  "NRM" -> "Normal"; "RPV" -> "Representative"
  -- Function
  "STA" -> "Stative"; "DYN" -> "Dynamic"
  -- Specification
  "BSC" -> "Basic"; "CTE" -> "Contential"
  "CSV" -> "Constitutive"; "OBJ" -> "Objective"
  -- Context
  "EXS" -> "Existential"; "FNC" -> "Functional"
  "RPS" -> "Representational"; "AMG" -> "Amalgamative"
  -- Stem
  "S1" -> "Stem 1"; "S2" -> "Stem 2"; "S3" -> "Stem 3"; "S0" -> "Stem 0"
  -- Version
  "PRC" -> "Processual"; "CPT" -> "Completive"
  -- Perspective
  "M_" -> "Monadic"; "G_" -> "Polyadic"; "N_" -> "Nomic"; "A_" -> "Abstract"
  other -> other

-- | Dump all grammar entries as formatted text
dumpGrammarTable :: Text -> Text
dumpGrammarTable category =
  let entries = if T.null category
        then grammarTable
        else filter (\e -> T.toCaseFold category `T.isInfixOf` T.toCaseFold (gCategory e)) grammarTable
      fmt e = T.justifyLeft 14 ' ' (gCategory e)
           <> T.justifyLeft 6 ' ' (gAbbrev e)
           <> T.justifyLeft 22 ' ' (gName e)
           <> gForm e
  in T.unlines (map fmt entries)

--------------------------------------------------------------------------------
-- Formative Composition
--------------------------------------------------------------------------------

-- | Compose a Formative into correctly-rendered Ithkuil text.
-- Uses Allomorph.constructCa for proper Ca forms, and applies stress marking.
-- When Slot V has affixes: geminates Ca to mark boundary; adds ' after Vv for 2+ affixes.
-- Prefers shortcut form (w/y prefix) when Ca is eligible and Vr is default.
composeFormative :: Formative -> Text
composeFormative f = applyStress (fStress f) unstressed
  where
    unstressed = case tryShortcut f of
      Just s  -> s
      Nothing -> composeFull f

-- | Full (non-shortcut) formative composition
composeFull :: Formative -> Text
composeFull f =
  let hasSlotV = not (null (fSlotV f))
      slotVMarker = if length (fSlotV f) >= 2 then "'" else ""
      ca = constructCa (fSlotVI f)
      caFinal = if hasSlotV then geminateCa ca else ca
  in T.concat
      [ renderSlotI (fSlotI f)
      , slotIIToVv (fSlotII f)
      , slotVMarker
      , renderRoot (fSlotIII f)
      , renderSlotIV (fSlotIV f)
      , renderSlotV (fSlotV f)
      , caFinal
      , renderSlotVII (fSlotVII f)
      , renderSlotVIII (fSlotVIII f)
      , renderSlotIX (fSlotIX f)
      ]

-- | Try to compose a formative using shortcut form (w/y prefix).
-- Returns Nothing if shortcut is not applicable.
-- Shortcut requires: no concatenation, default Vr (STA/BSC/EXS), no Slot V affixes,
-- and Ca must match one of the 8 shortcut patterns.
tryShortcut :: Formative -> Maybe Text
tryShortcut f = do
  -- No concatenation allowed
  case fSlotI f of
    Just _ -> Nothing
    Nothing -> Just ()
  -- Vr must be default (shortcuts elide Slot IV)
  case fSlotIV f of
    (STA, BSC, EXS) -> Just ()
    _ -> Nothing
  -- No Slot V affixes (shortcuts use the Slot V/VII region differently)
  case fSlotV f of
    [] -> Just ()
    _ -> Nothing
  -- Case vowel must not contain glottal stop (would be misinterpreted as slot V marker)
  let caseForm = renderSlotIX (fSlotIX f)
  if T.any (== '\'') caseForm then Nothing else Just ()
  -- Ca must match a shortcut pattern
  (prefix, series) <- shortcutFromCa (fSlotVI f)
  let vvForm = slotIIFormNum (fSlotII f)
      vv = vowelForm series vvForm
  Just $ T.concat
    [ prefix
    , vv
    , renderRoot (fSlotIII f)
    , renderSlotVII (fSlotVII f)
    , renderSlotVIII (fSlotVIII f)
    , renderSlotIX (fSlotIX f)
    ]

-- | Map Ca tuple to shortcut prefix ("w"/"y") and Vv series (1-4)
shortcutFromCa :: SlotVI -> Maybe (Text, Int)
shortcutFromCa (UNI, CSL, M_, DEL, NRM) = Just ("w", 1)  -- default Ca
shortcutFromCa (UNI, CSL, G_, DEL, NRM) = Just ("w", 2)  -- Polyadic
shortcutFromCa (UNI, CSL, N_, DEL, NRM) = Just ("w", 3)  -- Nomic
shortcutFromCa (UNI, CSL, G_, DEL, RPV) = Just ("w", 4)  -- Polyadic+RPV
shortcutFromCa (UNI, CSL, M_, PRX, NRM) = Just ("y", 1)  -- Proximal
shortcutFromCa (UNI, CSL, M_, DEL, RPV) = Just ("y", 2)  -- Representative
shortcutFromCa (UNI, CSL, A_, DEL, NRM) = Just ("y", 3)  -- Abstract
shortcutFromCa (UNI, CSL, M_, PRX, RPV) = Just ("y", 4)  -- Proximal+RPV
shortcutFromCa _ = Nothing

-- | Get the Vv form number for a stem/version pair
slotIIFormNum :: SlotII -> Int
slotIIFormNum (S1, PRC) = 1
slotIIFormNum (S1, CPT) = 2
slotIIFormNum (S2, PRC) = 3
slotIIFormNum (S2, CPT) = 4
slotIIFormNum (S3, PRC) = 9
slotIIFormNum (S3, CPT) = 8
slotIIFormNum (S0, PRC) = 7
slotIIFormNum (S0, CPT) = 6

-- | Compose a single referential: C1 + case vowel
-- Example: composeReferential (PersonalRef R1m NEU) (Transrelative ERG) = "lo" ("I" in ERG)
composeReferential :: PersonalRef -> Case -> Text
composeReferential ref c = refC1 ref <> renderCase c

-- | Geminate a Ca consonant cluster (inverse of degeminateCa).
-- Doubles the first consonant; uses special allomorphs for certain clusters.
geminateCa :: Text -> Text
geminateCa t =
  case lookup t caGemMap of
    Just gemmed -> gemmed
    Nothing ->
      -- Double the first character
      case T.uncons t of
        Just (c, rest) -> T.cons c (T.cons c rest)
        Nothing -> t

-- | Reverse map of caDegemMap from Parse.hs: normal Ca → geminated form
caGemMap :: [(Text, Text)]
caGemMap =
  [ ("dn", "jjn"), ("dm", "jjm")
  , ("gn", "gžžn"), ("gm", "gžžm"), ("bn", "bžžn"), ("bm", "bžžm")
  , ("tn", "ḑḑn"), ("tm", "ḑḑm"), ("kn", "xxn"), ("km", "xxm")
  , ("pn", "vvn"), ("pm", "vmm")
  , ("tp", "ddv"), ("tk", "ḑvv"), ("kp", "ggv"), ("kt", "ggḑ")
  , ("pk", "bbv"), ("pt", "bbḑ")
  ]

-- | Apply stress marking to an Ithkuil word.
-- Penultimate = no mark (default), Ultimate = acute on last vowel,
-- Antepenultimate = circumflex→umlaut on third-to-last vowel.
applyStress :: Stress -> Text -> Text
applyStress Penultimate t = t
applyStress Monosyllabic t = t
applyStress Ultimate t = accentNthNucleusFromEnd 1 t
applyStress Antepenultimate t = accentNthNucleusFromEnd 3 t

-- | Ithkuil diphthongs (treated as single syllable nuclei).
diphthongs :: [String]
diphthongs = ["ai","äi","ei","ëi","oi","öi","ui","au","eu","ëu","ou","iu"]

-- | Split a vowel group into syllable nuclei.
-- Diphthongs stay together; other multi-vowel sequences split into individual vowels.
splitNuclei :: String -> [String]
splitNuclei [] = []
splitNuclei [c] = [[c]]
splitNuclei (a:b:rest)
  | [a,b] `elem` diphthongs = [a,b] : splitNuclei rest
  | otherwise = [a] : splitNuclei (b:rest)

-- | Place acute accent on the first vowel of the Nth syllable nucleus from the end.
accentNthNucleusFromEnd :: Int -> Text -> Text
accentNthNucleusFromEnd n t =
  let chars = T.unpack t
      -- Find vowel group spans: (startPos, groupChars)
      groups = findVowelGroups chars 0
      -- Split each vowel group into nuclei, keeping track of start position
      nuclei = concatMap (\(pos, grp) -> assignPositions pos (splitNuclei grp)) groups
      targetIdx = length nuclei - n
  in if targetIdx < 0 || targetIdx >= length nuclei
     then t
     else let (pos, _) = nuclei !! targetIdx
              c' = acuteAccent (chars !! pos)
          in T.pack (take pos chars ++ [c'] ++ drop (pos + 1) chars)

-- | Find contiguous vowel groups with their starting positions.
findVowelGroups :: String -> Int -> [(Int, String)]
findVowelGroups [] _ = []
findVowelGroups (c:cs) pos
  | isVowel c = let (rest, remaining) = span isVowel cs
                    grp = c : rest
                in (pos, grp) : findVowelGroups remaining (pos + length grp)
  | otherwise = findVowelGroups cs (pos + 1)

-- | Assign character positions to nuclei within a vowel group.
assignPositions :: Int -> [String] -> [(Int, String)]
assignPositions _ [] = []
assignPositions pos (nuc:rest) = (pos, nuc) : assignPositions (pos + length nuc) rest

isVowel :: Char -> Bool
isVowel c = c `elem` ("aäeëiïoöuü" :: String)
         || c `elem` ("áàâãéèêíìîóòôúùû" :: String)  -- already accented

acuteAccent :: Char -> Char
acuteAccent 'a' = 'á'
acuteAccent 'ä' = 'â'  -- diaeresis + stress → circumflex
acuteAccent 'e' = 'é'
acuteAccent 'ë' = 'ê'  -- diaeresis + stress → circumflex
acuteAccent 'i' = 'í'
acuteAccent 'ï' = 'í'
acuteAccent 'o' = 'ó'
acuteAccent 'ö' = 'ô'  -- diaeresis + stress → circumflex
acuteAccent 'u' = 'ú'
acuteAccent 'ü' = 'û'  -- diaeresis + stress → circumflex
acuteAccent c = c  -- already accented or unknown
