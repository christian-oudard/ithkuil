{-# LANGUAGE OverloadedStrings #-}
-- | Common abstractions shared between Ithkuil V3 and V4.
--
-- Both versions share the same fundamental morphological architecture:
-- formatives with roots, Ca complexes, cases, and verbal categories.
-- The specific inventories differ but the structural patterns are the same.
--
-- Categories that are identical across V3/V4:
--   Affiliation (CSL, ASO, VAR, COA)
--   Perspective (M, U/G, N, A)
--   Extension (DEL, PRX, ICP, TRM/ATV, DPL/GRA, GRA/-)
--   Essence (NRM, RPV)
--   Context (EXS, FNC, RPS, AMG)
--   Valence (9 values, same names)
--   Phase (9 values, same names)
--
-- Categories that differ:
--   Function:      V3 has 4 (STA,DYN,MNF,DSC), V4 has 2 (STA,DYN)
--   Configuration: V3 has 9, V4 has 20 (adds similarity/separability)
--   Case:          V3 has 72+24, V4 has 68
--   Version:       V3 has 6 (tone), V4 has 2
--   Mood:          V3 has 8, V4 has 6
--   Illocution:    V3 has 6, V4 has 9
--   Stem:          V3 has 3, V4 has 4 (adds S0)
--   V3-only:       Pattern, Designation, Format, Sanction
--   V4-only:       Specification, Effect, Level, CaseScope
module Ithkuil.Common
  ( -- * Formative interface
    FormativeInfo(..)
  , toFormativeInfo
  , toFormativeInfoV3
    -- * Root
  , rootText
  , rootTextV3
    -- * Shared category names
  , affiliationName
  , perspectiveName
  , extensionName
  , essenceName
  , contextName
    -- * Cross-version grammar search
  , GrammarEntry(..)
  , LanguageVersion(..)
  , grammarTable
  , searchGrammar
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Ithkuil.Grammar as V4
import qualified Ithkuil.V3.Grammar as V3

-- | Version-independent formative summary.
-- Captures the structural information shared across all Ithkuil versions.
data FormativeInfo = FormativeInfo
  { fiRoot        :: Text          -- ^ Consonant root
  , fiStem        :: Int           -- ^ Stem number (1-3/4)
  , fiCaseLabel   :: Text          -- ^ Case abbreviation
  , fiVersion     :: Text          -- ^ Version name
  , fiDescription :: Text          -- ^ Human-readable summary
  }
  deriving (Show, Eq)

-- | Extract common info from a V4 formative
toFormativeInfo :: V4.Formative -> FormativeInfo
toFormativeInfo f = FormativeInfo
  { fiRoot = rootText (V4.fSlotIII f)
  , fiStem = stemNum (fst (V4.fSlotII f))
  , fiCaseLabel = case V4.fSlotIX f of
      Left c  -> v4CaseAbbrev c
      Right _ -> "Vk"
  , fiVersion = case snd (V4.fSlotII f) of
      V4.PRC -> "PRC"
      V4.CPT -> "CPT"
  , fiDescription = ""
  }
  where
    stemNum V4.S1 = 1
    stemNum V4.S2 = 2
    stemNum V4.S3 = 3
    stemNum V4.S0 = 0

-- | Extract common info from a V3 formative
toFormativeInfoV3 :: V3.Formative -> FormativeInfo
toFormativeInfoV3 f = FormativeInfo
  { fiRoot = rootTextV3 (V3.fRoot f)
  , fiStem = v3StemNum (let (_, _, s) = V3.fVr f in s)
  , fiCaseLabel = V3.caseAbbrev (V3.fCase f)
  , fiVersion = case V3.fTone f of
      V3.Falling       -> "PRC"
      V3.High          -> "CPT"
      V3.Rising        -> "INE"
      V3.Low           -> "INC"
      V3.FallingRising -> "PST"
      V3.RisingFalling -> "EFC"
  , fiDescription = ""
  }
  where
    v3StemNum V3.S1 = 1
    v3StemNum V3.S2 = 2
    v3StemNum V3.S3 = 3

-- | Extract text from a V4 root
rootText :: V4.Root -> Text
rootText (V4.Root t) = t

-- | Extract text from a V3 root
rootTextV3 :: V3.Root -> Text
rootTextV3 (V3.Root t) = t

--------------------------------------------------------------------------------
-- Shared category name rendering
--------------------------------------------------------------------------------

affiliationName :: Text -> Text
affiliationName "CSL" = "Consolidative"
affiliationName "ASO" = "Associative"
affiliationName "VAR" = "Variative"
affiliationName "COA" = "Coalescent"
affiliationName x     = x

perspectiveName :: Text -> Text
perspectiveName "M" = "Monadic"
perspectiveName "U" = "Unbounded"
perspectiveName "G" = "Agglomerative"
perspectiveName "N" = "Nomic"
perspectiveName "A" = "Abstract"
perspectiveName x   = x

extensionName :: Text -> Text
extensionName "DEL" = "Delimitive"
extensionName "PRX" = "Proximal"
extensionName "ICP" = "Incipient"
extensionName "TRM" = "Terminative"
extensionName "ATV" = "Attenuative"
extensionName "DPL" = "Depletive"
extensionName "GRA" = "Graduative"
extensionName x     = x

essenceName :: Text -> Text
essenceName "NRM" = "Normal"
essenceName "RPV" = "Representative"
essenceName x     = x

contextName :: Text -> Text
contextName "EXS" = "Existential"
contextName "FNC" = "Functional"
contextName "RPS" = "Representational"
contextName "AMG" = "Amalgamative"
contextName x     = x

--------------------------------------------------------------------------------
-- V4 case abbreviation helper (avoiding circular import)
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Cross-version grammar search
--------------------------------------------------------------------------------

data LanguageVersion = V3Only | V4Only | V3V4
  deriving (Show, Eq, Ord)

data GrammarEntry = GrammarEntry
  { geAbbrev   :: Text           -- ^ Abbreviation (e.g. "ERG")
  , geName     :: Text           -- ^ Full name
  , geCategory :: Text           -- ^ Category (e.g. "Case", "Mood")
  , geVersion  :: LanguageVersion -- ^ Which version(s)
  , geNote     :: Text           -- ^ Additional notes
  }
  deriving (Show, Eq)

-- | Comprehensive grammar table covering both V3 and V4 categories
grammarTable :: [GrammarEntry]
grammarTable = concat
  -- Shared categories (identical in both versions)
  [ entries V3V4 "Affiliation" [("CSL","Consolidative"),("ASO","Associative"),("VAR","Variative"),("COA","Coalescent")]
  , entries V3V4 "Essence" [("NRM","Normal"),("RPV","Representative")]
  , entries V3V4 "Context" [("EXS","Existential"),("FNC","Functional"),("RPS","Representational"),("AMG","Amalgamative")]

  -- Perspective: same concept, V3 uses M/U/N/A, V4 uses M/G/N/A
  , [GrammarEntry "M" "Monadic" "Perspective" V3V4 ""
    ,GrammarEntry "U" "Unbounded" "Perspective" V3Only "V4 equivalent: G (Agglomerative)"
    ,GrammarEntry "G" "Agglomerative" "Perspective" V4Only "V3 equivalent: U (Unbounded)"
    ,GrammarEntry "N" "Nomic" "Perspective" V3V4 ""
    ,GrammarEntry "A" "Abstract" "Perspective" V3V4 ""]

  -- Extension: mostly shared
  , entries V3V4 "Extension" [("DEL","Delimitive"),("PRX","Proximal"),("ICP","Incipient"),("DPL","Depletive"),("GRA","Graduative")]
  , [GrammarEntry "TRM" "Terminative" "Extension" V3Only "V4 equivalent: ATV (Attenuative)"
    ,GrammarEntry "ATV" "Attenuative" "Extension" V4Only "V3 equivalent: TRM (Terminative)"]

  -- Function: V3 has 4, V4 has 2
  , entries V3V4 "Function" [("STA","Stative"),("DYN","Dynamic")]
  , entries V3Only "Function" [("MNF","Manifestive"),("DSC","Descriptive")]

  -- V3-only categories
  , entries V3Only "Pattern" [("P1","Pattern 1"),("P2","Pattern 2"),("P3","Pattern 3")]
  , entries V3Only "Designation" [("FML","Formal"),("IFL","Informal")]
  , entries V3Only "Format" [("NOF","No Format"),("SCH","Schematic"),("ISR","Instrumental"),("ATH","Authoritative"),("RSL","Resultative"),("SBQ","Subsequent"),("CCM","Concomitant"),("OBJ","Objective"),("PRT","Precurrent"),("AFI","Affinitive")]

  -- V4-only categories
  , entries V4Only "Specification" [("BSC","Basic"),("CTE","Contential"),("CSV","Constitutive"),("OBJ","Objective")]
  , entries V4Only "Effect" [("BEN1","Beneficial-1"),("BEN2","Beneficial-2"),("BEN3","Beneficial-3"),("BSLF","Beneficial-self"),("UNK","Unknown"),("DSLF","Detrimental-self"),("DET3","Detrimental-3"),("DET2","Detrimental-2"),("DET1","Detrimental-1")]

  -- Version: V3 has 6 (tone), V4 has 2
  , entries V3V4 "Version" [("PRC","Processual"),("CPT","Completive")]
  , entries V3Only "Version" [("INE","Ineffective"),("INC","Incompletive"),("PST","Positive"),("EFC","Effective")]

  -- Stem: V3 has 3, V4 has 4
  , entries V3V4 "Stem" [("S1","Stem 1"),("S2","Stem 2"),("S3","Stem 3")]
  , entries V4Only "Stem" [("S0","Stem 0")]

  -- Mood: V3 has 8, V4 has 6
  , entries V3V4 "Mood" [("FAC","Factual"),("SUB","Subjunctive"),("ASM","Assumptive"),("SPC","Speculative"),("COU","Counterfactive"),("HYP","Hypothetical")]
  , entries V3Only "Mood" [("IPL","Implicative"),("ASC","Ascriptive")]
  ]
  where
    entries ver cat = map (\(a,n) -> GrammarEntry a n cat ver "")

-- | Search grammar entries by abbreviation or name (case-insensitive)
searchGrammar :: Text -> [GrammarEntry]
searchGrammar query = filter matches grammarTable
  where
    q = T.toLower query
    matches ge = q `T.isInfixOf` T.toLower (geAbbrev ge)
              || q `T.isInfixOf` T.toLower (geName ge)
              || q `T.isInfixOf` T.toLower (geCategory ge)

v4CaseAbbrev :: V4.Case -> Text
v4CaseAbbrev (V4.Transrelative c) = case c of
  V4.THM -> "THM"; V4.INS -> "INS"; V4.ABS -> "ABS"; V4.AFF -> "AFF"
  V4.STM -> "STM"; V4.EFF -> "EFF"; V4.ERG -> "ERG"; V4.DAT -> "DAT"
  V4.IND -> "IND"
v4CaseAbbrev (V4.Appositive c) = case c of
  V4.POS -> "POS"; V4.PRP -> "PRP"; V4.GEN -> "GEN"; V4.ATT -> "ATT"
  V4.PDC -> "PDC"; V4.ITP -> "ITP"; V4.OGN -> "OGN"; V4.IDP -> "IDP"
  V4.PAR -> "PAR"
v4CaseAbbrev (V4.Associative c) = case c of
  V4.APL -> "APL"; V4.PUR -> "PUR"; V4.TRA -> "TRA"; V4.DFR -> "DFR"
  V4.CRS -> "CRS"; V4.TSP -> "TSP"; V4.CMM -> "CMM"; V4.CMP -> "CMP"
  V4.CSD -> "CSD"
v4CaseAbbrev (V4.Adverbial c) = case c of
  V4.FUN -> "FUN"; V4.TFM -> "TFM"; V4.CLA -> "CLA"; V4.RSL -> "RSL"
  V4.CSM -> "CSM"; V4.CON -> "CON"; V4.AVR -> "AVR"; V4.CVS -> "CVS"
  V4.SIT -> "SIT"
v4CaseAbbrev (V4.Relational c) = case c of
  V4.PRN -> "PRN"; V4.DSP -> "DSP"; V4.COR -> "COR"; V4.CPS -> "CPS"
  V4.COM -> "COM"; V4.UTL -> "UTL"; V4.PRD -> "PRD"; V4.RLT -> "RLT"
v4CaseAbbrev (V4.Affinitive c) = case c of
  V4.ACT -> "ACT"; V4.ASI -> "ASI"; V4.ESS -> "ESS"; V4.TRM -> "TRM"
  V4.SEL -> "SEL"; V4.CFM -> "CFM"; V4.DEP -> "DEP"; V4.VOC -> "VOC"
v4CaseAbbrev (V4.SpatioTemporal1 c) = case c of
  V4.LOC -> "LOC"; V4.ATD -> "ATD"; V4.ALL -> "ALL"; V4.ABL -> "ABL"
  V4.ORI -> "ORI"; V4.IRL -> "IRL"; V4.INV -> "INV"; V4.NAV -> "NAV"
v4CaseAbbrev (V4.SpatioTemporal2 c) = case c of
  V4.CNR -> "CNR"; V4.ASS -> "ASS"; V4.PER -> "PER"; V4.PRO -> "PRO"
  V4.PCV -> "PCV"; V4.PCR -> "PCR"; V4.ELP -> "ELP"; V4.PLM -> "PLM"
