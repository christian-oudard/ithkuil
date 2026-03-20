{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V3 (Elartkha) Composition
-- Builds V3 formatives from grammar values and provides reverse lookups.
module Ithkuil.V3.Compose
  ( composeFormative
  , searchRoots
  , lookupGrammar
  , GrammarEntry(..)
  , grammarTable
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Ithkuil.V3.Grammar
import Ithkuil.V3.Render (renderFormative)
import Ithkuil.V3.Parse (CaTables)
import Ithkuil.V3.Lexicon (Lexicon)

-- | Compose a V3 formative to romanized text.
-- Convenience wrapper around renderFormative.
composeFormative :: CaTables -> Formative -> Maybe Text
composeFormative = renderFormative

-- | Search the V3 lexicon for roots matching a query (case-insensitive substring).
searchRoots :: Lexicon -> Text -> [(Text, Text)]
searchRoots lex' query =
  let q = T.toLower query
  in [ (root, def)
     | (root, def) <- Map.toList lex'
     , q `T.isInfixOf` T.toLower def || q `T.isInfixOf` root
     ]

-- | Grammar entry for reverse lookup.
data GrammarEntry = GrammarEntry
  { geAbbrev   :: Text
  , geName     :: Text
  , geCategory :: Text
  , geForm     :: Text  -- ^ Phonological form (vowel or consonant)
  }
  deriving (Show, Eq)

-- | Search the V3 grammar table by abbreviation or name.
lookupGrammar :: Text -> [GrammarEntry]
lookupGrammar query = filter matches grammarTable
  where
    q = T.toLower query
    matches ge = q `T.isInfixOf` T.toLower (geAbbrev ge)
              || q `T.isInfixOf` T.toLower (geName ge)
              || q `T.isInfixOf` T.toLower (geCategory ge)

-- | Complete V3 grammar table with forms.
grammarTable :: [GrammarEntry]
grammarTable = concat
  [ functionEntries, patternEntries, stemEntries
  , designationEntries, versionEntries
  , caseEntries, moodEntries, illocutionEntries
  , contextEntries, formatEntries
  , biasEntries, sanctionEntries
  ]

functionEntries :: [GrammarEntry]
functionEntries =
  [ GrammarEntry "STA" "Stative" "Function" ""
  , GrammarEntry "DYN" "Dynamic" "Function" ""
  , GrammarEntry "MNF" "Manifestive" "Function" ""
  , GrammarEntry "DSC" "Descriptive" "Function" ""
  ]

patternEntries :: [GrammarEntry]
patternEntries =
  [ GrammarEntry "P1" "Pattern 1" "Pattern" ""
  , GrammarEntry "P2" "Pattern 2" "Pattern" ""
  , GrammarEntry "P3" "Pattern 3" "Pattern" ""
  ]

stemEntries :: [GrammarEntry]
stemEntries =
  [ GrammarEntry "S1" "Stem 1" "Stem" ""
  , GrammarEntry "S2" "Stem 2" "Stem" ""
  , GrammarEntry "S3" "Stem 3" "Stem" ""
  ]

designationEntries :: [GrammarEntry]
designationEntries =
  [ GrammarEntry "IFL" "Informal" "Designation" "penultimate stress"
  , GrammarEntry "FML" "Formal" "Designation" "ultimate stress"
  ]

versionEntries :: [GrammarEntry]
versionEntries =
  [ GrammarEntry "PRC" "Processual" "Version" "falling tone"
  , GrammarEntry "CPT" "Completive" "Version" "high tone"
  , GrammarEntry "INE" "Ineffective" "Version" "rising tone"
  , GrammarEntry "INC" "Incompletive" "Version" "low tone"
  , GrammarEntry "PST" "Positive" "Version" "falling-rising tone"
  , GrammarEntry "EFC" "Effective" "Version" "rising-falling tone"
  ]

caseEntries :: [GrammarEntry]
caseEntries = map mkCase allCases
  where
    allCases = map Transrelative allOf ++ map Possessive allOf
            ++ map Associative allOf ++ map Adverbial allOf
            ++ map Relational allOf ++ map Temporal1 allOf
            ++ map Temporal2 allOf
            ++ [Compound g s | g <- allOf, s <- allOf]
    mkCase c = GrammarEntry (caseAbbrev c) "" "Case" ""

moodEntries :: [GrammarEntry]
moodEntries =
  [ GrammarEntry "FAC" "Factual" "Mood" ""
  , GrammarEntry "SUB" "Subjunctive" "Mood" ""
  , GrammarEntry "ASM" "Assumptive" "Mood" ""
  , GrammarEntry "SPC" "Speculative" "Mood" ""
  , GrammarEntry "COU" "Counterfactive" "Mood" ""
  , GrammarEntry "HYP" "Hypothetical" "Mood" ""
  , GrammarEntry "IPL" "Implicative" "Mood" ""
  , GrammarEntry "ASC" "Ascriptive" "Mood" ""
  ]

illocutionEntries :: [GrammarEntry]
illocutionEntries =
  [ GrammarEntry "ASR" "Assertive" "Illocution" ""
  , GrammarEntry "DIR" "Directive" "Illocution" ""
  , GrammarEntry "IRG" "Interrogative" "Illocution" ""
  , GrammarEntry "ADM" "Admonitive" "Illocution" ""
  , GrammarEntry "HOR" "Hortative" "Illocution" ""
  , GrammarEntry "DEC" "Declarative" "Illocution" ""
  ]

contextEntries :: [GrammarEntry]
contextEntries =
  [ GrammarEntry "EXS" "Existential" "Context" ""
  , GrammarEntry "FNC" "Functional" "Context" ""
  , GrammarEntry "RPS" "Representational" "Context" ""
  , GrammarEntry "AMG" "Amalgamative" "Context" ""
  ]

formatEntries :: [GrammarEntry]
formatEntries =
  [ GrammarEntry "NOF" "No Format" "Format" ""
  , GrammarEntry "SCH" "Schematic" "Format" ""
  , GrammarEntry "ISR" "Instrumental" "Format" ""
  , GrammarEntry "ATH" "Authoritative" "Format" ""
  , GrammarEntry "RSL" "Resultative" "Format" ""
  , GrammarEntry "SBQ" "Subsequent" "Format" ""
  , GrammarEntry "CCM" "Concomitant" "Format" ""
  , GrammarEntry "OBJ" "Objective" "Format" ""
  , GrammarEntry "PRT" "Precurrent" "Format" ""
  , GrammarEntry "AFI" "Affinitive" "Format" ""
  ]

biasEntries :: [GrammarEntry]
biasEntries = map mkBias (allOf :: [Bias])
  where
    mkBias b = GrammarEntry (T.pack (show b)) "" "Bias" ""

sanctionEntries :: [GrammarEntry]
sanctionEntries =
  [ GrammarEntry "PPS" "Propositional" "Sanction" ""
  , GrammarEntry "EPI" "Epistemic" "Sanction" ""
  , GrammarEntry "ALG" "Allegative" "Sanction" ""
  , GrammarEntry "IPU" "Imputative" "Sanction" ""
  , GrammarEntry "RFU" "Refutative" "Sanction" ""
  , GrammarEntry "REB" "Rebutative" "Sanction" ""
  , GrammarEntry "CJT" "Conjectural" "Sanction" ""
  , GrammarEntry "EXV" "Expatiative" "Sanction" ""
  , GrammarEntry "AXM" "Axiomatic" "Sanction" ""
  ]
