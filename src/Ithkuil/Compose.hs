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
  , SearchResults(..)
  , SearchHit(..)
  , AffixHit(..)
  , unifiedSearch
  , composeFormative
  , composeReferential
  , applyStress
  , slotIIFormNum
  ) where

import Data.List (sortBy, partition)
import Data.Function (on)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text.Stemming.English (stem)
import Ithkuil.Grammar
import Ithkuil.Render
import Ithkuil.Allomorph (constructCa, geminateCa)
import Ithkuil.Phonology (vowelForm)
import Ithkuil.Lexicon (RootEntry(..), AffixEntry(..), rootStem0, rootStem1, rootStem2, rootStem3)
import Ithkuil.Referentials (PersonalRef(..), refC1)

data GrammarEntry = GrammarEntry
  { gCategory :: Text    -- e.g. "Case", "Aspect"
  , gAbbrev   :: Text    -- e.g. "THM", "RTR"
  , gName     :: Text    -- e.g. "Thematic", "Retrospective"
  , gForm     :: Text    -- e.g. "a", "a" (vowel/consonant form)
  , gDescription :: Text -- e.g. "the party which undergoes..." (empty if none)
  , gGlosses  :: [Text]  -- e.g. ["at", "in", "on", "by"] (empty if none)
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
      idx = buildKeywordIndex roots
      keywordHits = lookupKeywordIndex q idx
      keywordResolved = [ (score, cr, entry)
                        | (score, cr) <- keywordHits
                        , Just entry <- [Map.lookup cr roots]
                        , cr /= stripped ]  -- exclude direct hit from keywords
      sorted = sortBy (compare `on` (\(s,cr,_) -> (s, T.length cr, cr))) keywordResolved
  in directHit ++ sorted

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
          -- Head-word bonus: first word in description is the primary concept
          firstWord = case extractStandaloneWords stripped of
            (fw:_) -> Set.fromList [fw, porterStem fw]
            []     -> Set.empty
          headBonus w = if w `Set.member` firstWord then -1 else 0
          -- Extract standalone words (not from hyphenated compounds)
          standaloneWs = extractStandaloneWords stripped
          standaloneSet = Set.fromList (standaloneWs ++ map porterStem standaloneWs)
          -- Words from hyphenated compounds get a penalty (like parentheticals)
          mainForms = Set.toList . Set.fromList $ ws ++ map porterStem ws
          compoundOnly = Set.toList $ Set.fromList mainForms `Set.difference` standaloneSet
          compoundScore = (pri + 2) * 10 + nWords  -- compound/slash words rank two stem levels down
          -- Parenthetical-only words score at full word count + 1
          allWs = extractWords desc
          nFull = length allWs
          parenScore = pri * 10 + nFull + 1
          parenOnly = Set.toList $ Set.fromList (allWs ++ map porterStem allWs)
                      `Set.difference` Set.fromList mainForms
      in [(w, [(max 0 (exactBonus w + headBonus w), cr)]) | w <- Set.toList standaloneSet]
         ++ [(w, [(compoundScore, cr)]) | w <- compoundOnly]
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
      filter (\w -> T.length w > 1 && not (isStopWord w)) .
      map (T.toCaseFold . T.dropWhile (\c -> not (c >= 'a' && c <= 'z'))) .
      filter (T.any (\c -> c >= 'a' && c <= 'z')) .
      T.words $ T.map (\c -> if c `elem` (".,;:!?()-/''\x2019" :: String) then ' ' else c) t

    isStopWord :: Text -> Bool
    isStopWord w = w `Set.member` stopWords

    stopWords :: Set.Set Text
    stopWords = Set.fromList
      ["the","an","of","or","to","as","in","on","at","by","for","its","via","per"]

    -- | Extract only standalone words (not sub-parts of hyphenated compounds,
    -- or slash-groups that modify a following word like "running/flowing water")
    -- "root-eating beetle" → ["beetle"], "running/flowing water" → ["water"]
    -- "a hill/mountain" → ["hill", "mountain"] (final slash group = alternatives)
    extractStandaloneWords :: Text -> [Text]
    extractStandaloneWords t =
      let -- Replace common punctuation (NOT "/" or "-") with spaces
          cleaned = T.map (\c -> if c `elem` (".,;:!?()'\x2019" :: String) then ' ' else c) t
          tokens = T.words cleaned
          -- Mark slash-tokens that are followed by another word as compounds
          tagged = zip tokens (drop 1 tokens ++ [""])
          isCompound tok nextTok = T.any (== '-') tok
            || (T.any (== '/') tok && not (T.null nextTok) && T.any isAlpha nextTok)
          isAlpha c = c >= 'a' && c <= 'z'
          -- Standalone tokens: no "-" and not a slash-modifier
          standaloneTokens = [tok | (tok, next) <- tagged, not (isCompound tok next)]
          -- Split on "/" to get individual words from surviving slash groups
      in filter (\w -> T.length w > 1 && not (isStopWord w)) .
         map (T.toCaseFold . T.dropWhile (\c -> not (isAlpha c))) .
         filter (T.any isAlpha) .
         concatMap (T.splitOn "/") $ standaloneTokens

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
        || T.toCaseFold (affixAbbrev entry) == q
        || T.isInfixOf q (T.toCaseFold (affixDesc entry))
        || any (T.isInfixOf q . T.toCaseFold) (affixDegrees entry)
  in filter matches (Map.toList affixes)

--------------------------------------------------------------------------------
-- Unified Dictionary Search
--------------------------------------------------------------------------------

-- | A hit in root search, categorized by where the match occurred
data SearchHit
  = InGloss Text RootEntry        -- ^ Cr matched a head-word/gloss token in stem0
  | InDefinition Text RootEntry    -- ^ Cr matched in a stem description
  deriving (Show)

-- | A hit in affix search, with matching degrees
data AffixHit = AffixHit
  { ahCs      :: Text
  , ahEntry   :: AffixEntry
  , ahDegrees :: [(Int, Text)]  -- ^ (degree number, text) of matching degrees
  } deriving (Show)

-- | Categorized search results across all data sources
data SearchResults = SearchResults
  { srGloss      :: [SearchHit]      -- ^ Roots where query matches a gloss word
  , srDefinition :: [SearchHit]      -- ^ Roots where query matches in a stem description
  , srAffix      :: [AffixHit]       -- ^ Affixes where query matches
  , srCase       :: [GrammarEntry]   -- ^ Cases where query matches description or glosses
  , srGrammar    :: [GrammarEntry]   -- ^ Other grammar entries matching by name
  } deriving (Show)

-- | Unified search across roots, affixes, cases, and grammar
unifiedSearch :: Text -> Map Text RootEntry -> Map Text AffixEntry -> SearchResults
unifiedSearch query roots affixes =
  let q = T.toCaseFold query
      qStemmed = stem Set.empty q
      -- Root search: categorize into gloss vs definition hits
      (glossHits, defHits) = categorizeRootHits q qStemmed roots
      -- Affix search: find matches and identify which degrees matched
      affixHits = searchAffixesDetailed q affixes
      -- Case search: match against descriptions and glosses
      caseHits = filter (caseMatches q) allCases
      -- Other grammar: match by name (but not cases, already covered)
      grammarHits = filter (\e -> not (isCaseCategory (gCategory e))
                                  && nameMatches q e) grammarTable
  in SearchResults glossHits defHits affixHits caseHits grammarHits

-- | Categorize root hits into gloss matches vs definition matches
categorizeRootHits :: Text -> Text -> Map Text RootEntry -> ([SearchHit], [SearchHit])
categorizeRootHits q qStemmed roots =
  let allRoots = Map.toList roots
      classify (cr, entry) =
        let s0 = T.toCaseFold (rootStem0 entry)
            glossTokens = extractGlossTokens s0
            glossStemmed = map (stem Set.empty) glossTokens
            isGlossMatch = any (\t -> t == q || t == qStemmed) glossTokens
                        || any (\t -> t == q || t == qStemmed) glossStemmed
            stems = [rootStem1 entry, rootStem2 entry, rootStem3 entry, rootStem0 entry]
            isDefMatch = any (T.isInfixOf q . T.toCaseFold) stems
            isCrMatch = T.toCaseFold cr == q
                     || T.toCaseFold cr == T.dropWhile (== '-') (T.dropWhileEnd (== '-') q)
        in if isCrMatch then Just (Left (InGloss cr entry))
           else if isGlossMatch then Just (Left (InGloss cr entry))
           else if isDefMatch then Just (Right (InDefinition cr entry))
           else Nothing
      results = concatMap (\r -> case classify r of
                  Just (Left h)  -> [(h, True)]
                  Just (Right h) -> [(h, False)]
                  Nothing        -> []) allRoots
      (glosses, defs) = partition snd results
  in (map fst glosses, map fst defs)

-- | Extract gloss tokens from stem0 description
-- Splits on "/" and extracts first word of each fragment, plus whole fragments
extractGlossTokens :: Text -> [Text]
extractGlossTokens desc =
  let parts = map T.strip (T.splitOn "/" desc)
      -- Single-word parts are direct glosses
      singleWords = filter (not . T.any (== ' ')) parts
      -- First word of multi-word parts
      headWords = concatMap (\p -> case T.words p of
                    (w:_) | T.length w > 1 -> [T.toCaseFold w]
                    _ -> []) parts
  in map T.toCaseFold singleWords ++ headWords

-- | Search affixes with detail about which degrees matched
searchAffixesDetailed :: Text -> Map Text AffixEntry -> [AffixHit]
searchAffixesDetailed q affixes =
  let stripped = T.dropWhile (== '-') . T.dropWhileEnd (== '-') $ q
      check (cs, entry) =
        let isCsMatch = T.toCaseFold cs == stripped
            isAbbrMatch = T.toCaseFold (affixAbbrev entry) == q
            isDescMatch = T.isInfixOf q (T.toCaseFold (affixDesc entry))
            matchingDegs = [(d, txt) | (d, txt) <- zip [1..] (affixDegrees entry)
                                     , T.isInfixOf q (T.toCaseFold txt)]
            isMatch = isCsMatch || isAbbrMatch || isDescMatch || not (null matchingDegs)
        in if isMatch
           then Just (AffixHit cs entry matchingDegs)
           else Nothing
  in concatMap (\r -> case check r of Just h -> [h]; Nothing -> []) (Map.toList affixes)

-- | Check if a case entry matches the query (in description or glosses)
caseMatches :: Text -> GrammarEntry -> Bool
caseMatches q e = isCaseCategory (gCategory e)
               && (T.isInfixOf q (T.toCaseFold (gDescription e))
                  || any (T.isInfixOf q . T.toCaseFold) (gGlosses e)
                  || T.toCaseFold (gAbbrev e) == q
                  || T.isInfixOf q (T.toCaseFold (gName e)))

-- | Check if a non-case grammar entry matches by name
nameMatches :: Text -> GrammarEntry -> Bool
nameMatches q e = T.toCaseFold (gAbbrev e) == q
               || T.isInfixOf q (T.toCaseFold (gName e))

isCaseCategory :: Text -> Bool
isCaseCategory cat = "Case/" `T.isPrefixOf` cat

-- | Complete grammar table for reverse lookup
grammarTable :: [GrammarEntry]
grammarTable = concat
  [ allCases, allValences, allAspects, allPhases, allLevels, allEffects
  , allMoods, allIllocutions, allValidations, allCaseScopes
  , allConfigurations, allAffiliations, allPerspectives, allExtensions, allEssences
  , allFunctions, allSpecifications, allContexts, allStems, allVersions
  , allCaseAccessorCs
  ]

-- Cases (68) with descriptions and glosses from grammar reference
allCases :: [GrammarEntry]
allCases = concat [transrelativeCases, appositiveCases, associativeCases, adverbialCases
                  , relationalCases, affinitiveCases, spatioTemporal1Cases, spatioTemporal2Cases]

transrelativeCases :: [GrammarEntry]
transrelativeCases =
  [ caseEntry "Case/Trans" THM "Thematic" (Transrelative THM)
    "The (usually inanimate) party which is a participant to the verbal predicate which does not undergo any tangible change of state. Semantic role: CONTENT."
    ["content", "theme", "about"]
  , caseEntry "Case/Trans" INS "Instrumental" (Transrelative INS)
    "The entity acting as means utilized by an explicit or implicit agent to implement/carry out the effect/impact of an act/event. Semantic role: INSTRUMENT."
    ["by", "with", "using", "via"]
  , caseEntry "Case/Trans" ABS "Absolutive" (Transrelative ABS)
    "The party that/who is the target of, and/or undergoes, the effect/impact or change of state as a result of a tangible act/event. Semantic role: PATIENT."
    ["patient"]
  , caseEntry "Case/Trans" AFF "Affective" (Transrelative AFF)
    "The party who undergoes an unwilled, affective experience, e.g., coughing, sneezing, feeling hot/cold, trembling, experiencing sensory input. Semantic role: EXPERIENCER."
    ["experiencer"]
  , caseEntry "Case/Trans" STM "Stimulative" (Transrelative STM)
    "The party/entity/idea/thought/situation or mental state which triggers an unwilled, affective response or is the trigger for an existential state such as possession. Semantic role: STIMULUS."
    ["stimulus", "trigger"]
  , caseEntry "Case/Trans" EFF "Effectuative" (Transrelative EFF)
    "The party/force that initiates a chain of causal events or who induces another party to act as an agent. Semantic role: ENABLER."
    ["enabler", "cause"]
  , caseEntry "Case/Trans" ERG "Ergative" (Transrelative ERG)
    "The animate party or inanimate force which initiates/causes an act/event which creates a tangible effect or change of state in a patient. Semantic role: AGENT or FORCE."
    ["agent", "force", "doer"]
  , caseEntry "Case/Trans" DAT "Dative" (Transrelative DAT)
    "The party which is the (intended) recipient of a verb of transference, transmission, or communication. Semantic role: RECIPIENT."
    ["to", "recipient"]
  , caseEntry "Case/Trans" IND "Inducive" (Transrelative IND)
    "The patient who undergoes the tangible effect initiated/caused by that self-same party. Semantic role: AGENT+PATIENT."
    ["self-acting"]
  ]

appositiveCases :: [GrammarEntry]
appositiveCases =
  [ caseEntry "Case/Appos" POS "Possessive" (Appositive POS)
    "The party which has alienable (i.e., removable or severable) possession of another noun in the sense of having physical control or oversight."
    ["possessing", "holding", "having"]
  , caseEntry "Case/Appos" PRP "Proprietive" (Appositive PRP)
    "The party having alienable possession in the sense of quasi-permanent contextual control, ownership or oversight, by societal recognition, social convention, law, purchase or decree."
    ["belonging to", "owned by"]
  , caseEntry "Case/Appos" GEN "Genitive" (Appositive GEN)
    "The party which has inalienable (i.e., irremovable, non-severable) possession of, or association with another noun as an inherent attribute, physical part, or genetic bond."
    ["of", "'s"]
  , caseEntry "Case/Appos" ATT "Attributive" (Appositive ATT)
    "The party which inalienably experiences the effects of, or has an affective relationship with another noun, as a temporary or permanent attribute or characteristic."
    ["characterized by"]
  , caseEntry "Case/Appos" PDC "Productive" (Appositive PDC)
    "The party which is the creator, author or originator of another noun."
    ["by", "created by", "authored by"]
  , caseEntry "Case/Appos" ITP "Interpretive" (Appositive ITP)
    "The party acting as the subjective interpretational context of another noun, the party through which another noun is subjectively considered or described."
    ["according to", "as seen by"]
  , caseEntry "Case/Appos" OGN "Originative" (Appositive OGN)
    "The party which is the literal or figurative source of another, or the native location, origin, or usual locative context."
    ["from", "originating from"]
  , caseEntry "Case/Appos" IDP "Interdependent" (Appositive IDP)
    "The party which has a coordinated, tandem, complementary or mutually dependent relationship with another."
    ["complementary to", "paired with"]
  , caseEntry "Case/Appos" PAR "Partitive" (Appositive PAR)
    "Indicates a quantitative or content-to-container relationship between two nouns, e.g., a cup of coffee, a box of books."
    ["of", "-ful of"]
  ]

associativeCases :: [GrammarEntry]
associativeCases =
  [ caseEntry "Case/Assoc" APL "Applicative" (Associative APL)
    "The entity/act/event which constitutes the circumstantial, potentially one-time, temporary purpose/function/use to which X is circumstantially put."
    ["temporarily as", "used as"]
  , caseEntry "Case/Assoc" PUR "Purposive" (Associative PUR)
    "The entity/act/event which constitutes the inherent/innate/intrinsic purpose of another entity/act/event."
    ["for", "for the purpose of"]
  , caseEntry "Case/Assoc" TRA "Transmissive" (Associative TRA)
    "The party for which/whom an entity/act/event occurs or is transmitted, with the intention that it be beneficial/detrimental."
    ["for", "for the benefit of"]
  , caseEntry "Case/Assoc" DFR "Deferential" (Associative DFR)
    "The entity for whose sake, or out of deference/respect to whom, an act/event occurs."
    ["for the sake of", "out of respect for"]
  , caseEntry "Case/Assoc" CRS "Contrastive" (Associative CRS)
    "The party for which something is substituted, or of which another party takes its place."
    ["instead of", "in place of"]
  , caseEntry "Case/Assoc" TSP "Transpositive" (Associative TSP)
    "The party on whose behalf something is/occurs."
    ["on behalf of"]
  , caseEntry "Case/Assoc" CMM "Commutative" (Associative CMM)
    "The party in exchange for which, a reciprocal or complementary act/event occurs."
    ["in exchange for"]
  , caseEntry "Case/Assoc" CMP "Comparative" (Associative CMP)
    "The party being compared to another, translatable as 'as compared to,' 'versus'."
    ["compared to", "versus", "whereas"]
  , caseEntry "Case/Assoc" CSD "Considerative" (Associative CSD)
    "The entity according to which, another entity is, or an act/event occurs."
    ["according to"]
  ]

adverbialCases :: [GrammarEntry]
adverbialCases =
  [ caseEntry "Case/Advrb" FUN "Functive" (Adverbial FUN)
    "Identifies a noun used to describe/characterize the manner in which an act/event/state occurs or exists, e.g., She dances gracefully."
    ["manner of", "-ly", "with"]
  , caseEntry "Case/Advrb" TFM "Transformative" (Adverbial TFM)
    "Identifies the outcome or final state of a process, often translatable by 'to,' 'until,' or 'into' in the sense of reaching a final state."
    ["into", "to", "until"]
  , caseEntry "Case/Advrb" CLA "Classificative" (Adverbial CLA)
    "Identifies a noun as a basis for arranging, sorting, classifying, or counting, e.g., by color, in rows, by fives."
    ["by", "in groups of", "sorted by"]
  , caseEntry "Case/Advrb" RSL "Resultative" (Adverbial RSL)
    "Identifies a result/consequence, translatable as 'resulting in X', 'with X as a consequence'."
    ["resulting in", "consequently"]
  , caseEntry "Case/Advrb" CSM "Consumptive" (Adverbial CSM)
    "Identifies the entity consumed or used as a resource as part of a process, e.g., She cooks with tomatoes, He reads by candlelight."
    ["consuming", "using up"]
  , caseEntry "Case/Advrb" CON "Concessive" (Adverbial CON)
    "Identifies a noun which gives rise to an implicitly expected result which does not occur. Translates 'despite,' 'in spite of,' 'notwithstanding,' 'although.'"
    ["despite", "in spite of", "although", "regardless of"]
  , caseEntry "Case/Advrb" AVR "Aversive" (Adverbial AVR)
    "Identifies a source or object of fear and/or avoidance. Translates 'for fear of,' 'in order to avoid,' 'lest.'"
    ["for fear of", "to avoid", "lest"]
  , caseEntry "Case/Advrb" CVS "Conversive" (Adverbial CVS)
    "Identifies an exception. Translates 'unless,' 'except for,' 'but for,' 'excluding,' 'if not for.'"
    ["except", "unless", "but for", "excluding"]
  , caseEntry "Case/Advrb" SIT "Situative" (Adverbial SIT)
    "Identifies a noun as the background context for a clause without implying direct causation. Translates 'because of,' 'given,' 'considering,' 'in view of.'"
    ["because of", "given", "considering"]
  ]

relationalCases :: [GrammarEntry]
relationalCases =
  [ caseEntry "Case/Relat" PRN "Pertinential" (Relational PRN)
    "Identifies the general referent of another formative. Translates 'about,' 'regarding,' 'concerning,' 'pertaining to.'"
    ["about", "regarding", "concerning"]
  , caseEntry "Case/Relat" DSP "Descriptive" (Relational DSP)
    "Identifies a formative as describing another in an adjectival manner, translatable as 'characterized as being like X.'"
    ["like", "characterized as"]
  , caseEntry "Case/Relat" COR "Correlative" (Relational COR)
    "Indicates an abstract general relationship, association, or conjunction between formatives, including metaphorical or symbolic associations. Translates 'relative to,' 'in relation to.'"
    ["relative to", "in relation to"]
  , caseEntry "Case/Relat" CPS "Compositive" (Relational CPS)
    "Identifies a noun as being the literal or figurative substance or component of which another is made. E.g., carved out of marble, golden coins, a web of lies."
    ["made of", "composed of", "out of"]
  , caseEntry "Case/Relat" COM "Comitative" (Relational COM)
    "Identifies a formative that accompanies another. Translates '(along) with.'"
    ["with", "along with", "together with"]
  , caseEntry "Case/Relat" UTL "Utilitative" (Relational UTL)
    "Identifies a formative in the process of being used while some other activity or state is in progress. E.g., the gun-wielding man, the umbrella-toting pedestrian."
    ["wielding", "bearing", "wearing"]
  , caseEntry "Case/Relat" PRD "Predicative" (Relational PRD)
    "Identifies the non-causal basis, foundation, sustenance, or required existential condition for another noun. Translates 'based on,' 'dependent on,' 'relying on.'"
    ["based on", "dependent on", "relying on"]
  , caseEntry "Case/Relat" RLT "Relative" (Relational RLT)
    "Identifies a formative or case-frame as constituting a relative clause associated with the preceding formative."
    ["which", "that", "who"]
  ]

affinitiveCases :: [GrammarEntry]
affinitiveCases =
  [ caseEntry "Case/Affin" ACT "Activative" (Affinitive ACT)
    "Identifies the EXPERIENCER of a modal state, such as obligation, necessity, desire, hope, expectation, functioning as the subject of a modalized verb."
    ["modal experiencer"]
  , caseEntry "Case/Affin" ASI "Assimilative" (Affinitive ASI)
    "Identifies a formative used as a context for analogy or metaphorical comparison. Translates 'as/like' meaning 'as if it were X.'"
    ["as if", "like"]
  , caseEntry "Case/Affin" ESS "Essive" (Affinitive ESS)
    "Identifies the role or name by which an entity is known or contextually identified. Translates 'as/like' in the sense of naming or functional identity."
    ["as", "in the role of", "qua"]
  , caseEntry "Case/Affin" TRM "Terminative" (Affinitive TRM)
    "Identifies a noun as being the goal of an act/event, e.g., We seek a new planet; a desire to see his homeland."
    ["toward", "goal of", "seeking"]
  , caseEntry "Case/Affin" SEL "Selective" (Affinitive SEL)
    "Identifies a contextually recurring time-period or string of recurring entities, e.g., every three days, every Sunday."
    ["every", "each", "recurring"]
  , caseEntry "Case/Affin" CFM "Conformative" (Affinitive CFM)
    "The entity pursuant to which, as per which, or in conformance with which, another entity is, or an act/event occurs."
    ["pursuant to", "as per", "in conformance with"]
  , caseEntry "Case/Affin" DEP "Dependent" (Affinitive DEP)
    "Identifies a formative as being the basis of a dependency phrase on which another formative acts as the contingency."
    ["if", "contingent on"]
  , caseEntry "Case/Affin" VOC "Vocative" (Affinitive VOC)
    "Signifies a noun being used in direct address."
    ["O", "hey"]
  ]

spatioTemporal1Cases :: [GrammarEntry]
spatioTemporal1Cases =
  [ caseEntry "Case/SptTm1" LOC "Locative" (SpatioTemporal1 LOC)
    "Entity identified as the location where something is situated or occurs."
    ["at", "in", "on", "by"]
  , caseEntry "Case/SptTm1" ATD "Attendant" (SpatioTemporal1 ATD)
    "Entity in whose presence something is/occurs and which is thereby involved in some peripheral manner (e.g., as a witness)."
    ["in the presence of", "before"]
  , caseEntry "Case/SptTm1" ALL "Allative" (SpatioTemporal1 ALL)
    "Entity toward which another entity is moving/approaching."
    ["toward", "to"]
  , caseEntry "Case/SptTm1" ABL "Ablative" (SpatioTemporal1 ABL)
    "Entity away from which another entity is moving/receding."
    ["from", "away from"]
  , caseEntry "Case/SptTm1" ORI "Orientative" (SpatioTemporal1 ORI)
    "Entity (often a body part or sub-component) which serves as the 'face' or surface or 'front' in terms of external communication or directional movement."
    ["facing", "oriented toward"]
  , caseEntry "Case/SptTm1" IRL "Interrelative" (SpatioTemporal1 IRL)
    "Signifies the directional/temporal orientation or position of an entity relative to another, e.g., west of the house, relative to my arrival."
    ["relative to", "in relation to"]
  , caseEntry "Case/SptTm1" INV "Intrative" (SpatioTemporal1 INV)
    "Identifies a noun as being the spatio-temporal boundary point of a span of space or period of time. Translates 'since,' 'until,' 'as of.'"
    ["since", "until", "as of"]
  , caseEntry "Case/SptTm1" NAV "Navigative" (SpatioTemporal1 NAV)
    "Entity whose literal or metaphorically inferred long axis serves as the direction of another entity's path or trajectory."
    ["along", "following the path of"]
  ]

spatioTemporal2Cases :: [GrammarEntry]
spatioTemporal2Cases =
  [ caseEntry "Case/SptTm2" CNR "Concursive" (SpatioTemporal2 CNR)
    "Indicates a 'temporal locative' meaning 'at or during the time of X,' the point or stretch in time at or during which something exists or occurs."
    ["during", "at the time of"]
  , caseEntry "Case/SptTm2" ASS "Assessive" (SpatioTemporal2 ASS)
    "Specifies the increment of space or time by which a contextual ratio of measurement is created, e.g., by the minute, per hour, per book."
    ["per", "by the"]
  , caseEntry "Case/SptTm2" PER "Periodic" (SpatioTemporal2 PER)
    "Identifies the span of time at some point during which separate events or durationally segmented acts take place, e.g., in six months, within a few days."
    ["in", "over", "within"]
  , caseEntry "Case/SptTm2" PRO "Prolapsive" (SpatioTemporal2 PRO)
    "Signifies the duration of an act, condition, or event, i.e., how long it takes or lasts, e.g., through lunch, all night, for an hour."
    ["for", "through", "lasting"]
  , caseEntry "Case/SptTm2" PCV "Precursive" (SpatioTemporal2 PCV)
    "Identifies an entity acting as a point in time prior to which an act, condition, or event occurs."
    ["before", "prior to", "preceding"]
  , caseEntry "Case/SptTm2" PCR "Postcursive" (SpatioTemporal2 PCR)
    "Identifies an entity acting as a point in time after which, following which, subsequent to which, an act or event occurs."
    ["after", "following", "since"]
  , caseEntry "Case/SptTm2" ELP "Elapsive" (SpatioTemporal2 ELP)
    "Identifies the amount of time that has passed or is expected to pass between the contextual present and the time of the act/event, e.g., four years ago, in three days."
    ["ago", "from now"]
  , caseEntry "Case/SptTm2" PLM "Prolimitive" (SpatioTemporal2 PLM)
    "Signifies the spatio-temporal boundary point within which something is expected to occur or be situated. Translates 'by the time of,' 'within,' 'before X is over.'"
    ["by", "within", "in time for"]
  ]

-- | Helper to build a case entry with description and glosses
caseEntry :: Show a => Text -> a -> Text -> Case -> Text -> [Text] -> GrammarEntry
caseEntry cat constructor name_ c desc glosses =
  geDesc cat (sn constructor) name_ (renderCase c) desc glosses

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
  , ge "Perspective" "G" "Agglomerative" "r"
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

allCaseAccessorCs :: [GrammarEntry]
allCaseAccessorCs =
  [ ge "Affix/CaseAcc" "acc₁" "Type-1 Case-Accessor (cases 1-36)" "sw"
  , ge "Affix/CaseAcc" "acc₁" "Type-1 Case-Accessor (cases 37-68)" "sy"
  , ge "Affix/CaseAcc" "acc₂" "Type-2 Case-Accessor (cases 1-36)" "zw"
  , ge "Affix/CaseAcc" "acc₂" "Type-2 Case-Accessor (cases 37-68)" "zy"
  , ge "Affix/CaseAcc" "acc₃" "Type-3 Case-Accessor (cases 1-36)" "čw"
  , ge "Affix/CaseAcc" "acc₃" "Type-3 Case-Accessor (cases 37-68)" "čy"
  , ge "Affix/InvAcc" "ia₁" "Type-1 Inverse Accessor (cases 1-36)" "šw"
  , ge "Affix/InvAcc" "ia₁" "Type-1 Inverse Accessor (cases 37-68)" "šy"
  , ge "Affix/InvAcc" "ia₂" "Type-2 Inverse Accessor (cases 1-36)" "žw"
  , ge "Affix/InvAcc" "ia₂" "Type-2 Inverse Accessor (cases 37-68)" "žy"
  , ge "Affix/InvAcc" "ia₃" "Type-3 Inverse Accessor (cases 1-36)" "jw"
  , ge "Affix/InvAcc" "ia₃" "Type-3 Inverse Accessor (cases 37-68)" "jy"
  , ge "Affix/CaseStk" "case" "Case-Stacking (cases 1-36)" "lw"
  , ge "Affix/CaseStk" "case" "Case-Stacking (cases 37-68)" "ly"
  ]

-- Helpers
ge :: Text -> Text -> Text -> Text -> GrammarEntry
ge cat abbr name_ form_ = GrammarEntry cat abbr name_ form_ "" []

-- | Grammar entry with description and glosses
geDesc :: Text -> Text -> Text -> Text -> Text -> [Text] -> GrammarEntry
geDesc = GrammarEntry

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
  "PCT" -> "Punctual"; "ITR" -> "Iterative"; "REP" -> "Repetitive"
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
  "M_" -> "Monadic"; "G_" -> "Agglomerative"; "N_" -> "Nomic"; "A_" -> "Abstract"
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
  -- Antepenultimate stress (FRA) requires 3+ syllables.
  -- Shortcut base is 2 syllables; each Slot VII affix or Slot VIII adds 1.
  let extraSyllables = length (fSlotVII f) + (case fSlotVIII f of Just _ -> 1; Nothing -> 0)
  case fStress f of
    Antepenultimate | extraSyllables == 0 -> Nothing
    _ -> Just ()
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
shortcutFromCa (UNI, CSL, G_, DEL, NRM) = Just ("w", 2)  -- Agglomerative
shortcutFromCa (UNI, CSL, N_, DEL, NRM) = Just ("w", 3)  -- Nomic
shortcutFromCa (UNI, CSL, G_, DEL, RPV) = Just ("w", 4)  -- Agglomerative+RPV
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
