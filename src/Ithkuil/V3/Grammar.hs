{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V3 (Elartkha) Morphological Grammar
--
-- V3 formative structure (simplified):
--   Vr + Cr + Vc (+Ci+Vi) + Ca (+Vf (+Cb)) [+Tone] [+Stress]
--
-- Full structure:
--   (((Cv+)Vl+)Cg\/Cs+) Vr + (Cx\/Cv+Vp\/Vl+) Cr + Vc (+Ci+Vi) + Ca (+Vx+Cx) (+Vf(+Cb)) [+Tone] [+Stress]
--
-- Key differences from V4:
--   - 4 Functions (STA, DYN, MNF, DSC) vs V4's 2
--   - 3 Patterns (P1, P2, P3) instead of V4's Specification
--   - 6 Versions (tone-encoded) vs V4's 2
--   - 2 Designations (stress-encoded) vs V4 removing this
--   - 9 Configurations (no similarity/separability subdivision) vs V4's 20
--   - 8 Moods (includes IPL, ASC) vs V4's 6
--   - 72 base cases + 24 compound cases vs V4's 68
--   - 10 Formats (V4 removes this)
--   - Tone encodes Version; Stress encodes Designation + Relation
module Ithkuil.V3.Grammar where

import Data.Text (Text)

-- | Helper for bounded enumerations
allOf :: (Enum a, Bounded a) => [a]
allOf = [minBound .. maxBound]

--------------------------------------------------------------------------------
-- Function (Slot Vr component)
-- V3 has 4 functions vs V4's 2
--------------------------------------------------------------------------------

data Function
  = STA  -- ^ Stative
  | DYN  -- ^ Dynamic
  | MNF  -- ^ Manifestive
  | DSC  -- ^ Descriptive
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Pattern (Slot Vr component, replaces V4's Specification)
--------------------------------------------------------------------------------

data Pattern = P1 | P2 | P3
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Stem
--------------------------------------------------------------------------------

data Stem = S1 | S2 | S3
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Designation (encoded via stress)
--------------------------------------------------------------------------------

data Designation = FML | IFL  -- ^ Formal / Informal
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Relation (encoded via stress, with Designation)
--------------------------------------------------------------------------------

data Relation = Unframed | Framed
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Version (encoded via tone, 6 values vs V4's 2)
--------------------------------------------------------------------------------

data Version
  = PRC  -- ^ Processual (falling tone)
  | CPT  -- ^ Completive (high tone)
  | INE  -- ^ Ineffective (rising tone)
  | INC  -- ^ Incompletive (low tone)
  | PST  -- ^ Positive (falling-rising tone)
  | EFC  -- ^ Effective (rising-falling tone)
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Context
--------------------------------------------------------------------------------

data Context
  = EXS  -- ^ Existential
  | FNC  -- ^ Functional
  | RPS  -- ^ Representational
  | AMG  -- ^ Amalgamative
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Format (V3-specific, removed in V4)
--------------------------------------------------------------------------------

data Format
  = NOF  -- ^ No format (default)
  | SCH  -- ^ Schematic
  | ISR  -- ^ Instrumental
  | ATH  -- ^ Authoritative
  | RSL  -- ^ Resultative
  | SBQ  -- ^ Subsequent
  | CCM  -- ^ Concomitant
  | OBJ  -- ^ Objective
  | PRT  -- ^ Precurrent
  | AFI  -- ^ Affinitive
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Configuration (9 values vs V4's 20)
-- V3 does not subdivide by similarity/separability
--------------------------------------------------------------------------------

data Configuration
  = UNI  -- ^ Uniplex
  | DPX  -- ^ Duplex
  | DCT  -- ^ Discrete
  | AGG  -- ^ Aggregative
  | SEG  -- ^ Segmentative
  | CPN  -- ^ Componential
  | COH  -- ^ Coherent
  | CST  -- ^ Composite
  | MLT  -- ^ Multiform
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Affiliation (shared with V4)
--------------------------------------------------------------------------------

data Affiliation
  = CSL  -- ^ Consolidative
  | ASO  -- ^ Associative
  | VAR  -- ^ Variative
  | COA  -- ^ Coalescent
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Perspective (shared with V4, different abbreviations)
--------------------------------------------------------------------------------

data Perspective
  = M_  -- ^ Monadic
  | U_  -- ^ Unbounded
  | N_  -- ^ Nomic
  | A_  -- ^ Abstract
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Extension (shared with V4, slightly different names)
--------------------------------------------------------------------------------

data Extension
  = DEL  -- ^ Delimitive
  | PRX  -- ^ Proximal
  | ICP  -- ^ Incipient
  | TRM  -- ^ Terminative
  | DPL  -- ^ Depletive
  | GRA  -- ^ Graduative
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Essence (shared with V4)
--------------------------------------------------------------------------------

data Essence
  = NRM  -- ^ Normal
  | RPV  -- ^ Representative
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Ca Complex
--------------------------------------------------------------------------------

type CaComplex = (Essence, Extension, Perspective, Affiliation, Configuration)

defaultCa :: CaComplex
defaultCa = (NRM, DEL, M_, CSL, UNI)

--------------------------------------------------------------------------------
-- Cases (72 base + 24 compound = 96 total)
--------------------------------------------------------------------------------

-- | Transrelative Cases (10)
data TransrelativeCase
  = OBL  -- ^ Oblique
  | IND  -- ^ Inducive
  | ABS  -- ^ Absolutive
  | ERG  -- ^ Ergative
  | EFF  -- ^ Effectuative
  | AFF  -- ^ Affective
  | DAT  -- ^ Dative
  | INS  -- ^ Instrumental
  | ACT  -- ^ Activative
  | DER  -- ^ Derivative
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Possessive Cases (10)
data PossessiveCase
  = SIT  -- ^ Situative
  | POS  -- ^ Possessive
  | PRP  -- ^ Proprietive
  | GEN  -- ^ Genitive
  | ATT  -- ^ Attributive
  | PDC  -- ^ Productive
  | ITP  -- ^ Interpretive
  | OGN  -- ^ Originative
  | PAR  -- ^ Partitive
  | CRS  -- ^ Contrastive
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Associative Cases (10)
data AssociativeCase
  = CPS  -- ^ Compositive
  | PRD  -- ^ Predicative
  | MED  -- ^ Mediative
  | APL  -- ^ Applicative
  | PUR  -- ^ Purposive
  | CSD  -- ^ Considerative
  | ESS  -- ^ Essive
  | ASI  -- ^ Assimilative
  | FUN  -- ^ Functive
  | TFM  -- ^ Transformative
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Adverbial Cases (10)
data AdverbialCase
  = REF  -- ^ Referential
  | CLA  -- ^ Classificative
  | CNV  -- ^ Conversive
  | IDP  -- ^ Interdependent
  | BEN  -- ^ Benefactive
  | TSP  -- ^ Transpositive
  | CMM  -- ^ Commutative
  | COM  -- ^ Comitative
  | CNJ  -- ^ Conjunctive
  | UTL  -- ^ Utilitative
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Relational Cases (10)
data RelationalCase
  = ABE  -- ^ Aversive
  | CVS  -- ^ Conversive
  | COR  -- ^ Correlative
  | DEP  -- ^ Dependent
  | PVS  -- ^ Provisional
  | PTL  -- ^ Postulative
  | CON  -- ^ Concessive
  | EXC  -- ^ Exceptive
  | AVR  -- ^ Aversive
  | CMP  -- ^ Comparative
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Temporal Cases I (12)
data TemporalCase1
  = SML  -- ^ Semblative
  | ASS  -- ^ Assessive
  | CNR  -- ^ Concursive
  | ACS  -- ^ Accessive
  | DFF  -- ^ Diffusive
  | PER  -- ^ Periodic
  | PRO  -- ^ Prolapsive
  | PCV  -- ^ Precursive
  | PCR  -- ^ Postcursive
  | ELP  -- ^ Elapsive
  | ALP  -- ^ Allapsive
  | INP  -- ^ Interpolative
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Temporal/Spatial Cases II (10)
data TemporalCase2
  = EPS  -- ^ Episodic
  | PLM  -- ^ Prolimitive
  | LMT  -- ^ Limitative
  | LOC  -- ^ Locative
  | ORI  -- ^ Orientative
  | PSV  -- ^ Procursive
  | ALL  -- ^ Allative
  | ABL  -- ^ Ablative
  | NAV  -- ^ Navigative
  | VOC  -- ^ Vocative
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Compound case series
data CompoundSeries = CmpA | CmpB | CmpC
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Compound case group (1-8)
data CompoundGroup = Cmp1 | Cmp2 | Cmp3 | Cmp4 | Cmp5 | Cmp6 | Cmp7 | Cmp8
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | All cases unified
data Case
  = Transrelative TransrelativeCase
  | Possessive PossessiveCase
  | Associative AssociativeCase
  | Adverbial AdverbialCase
  | Relational RelationalCase
  | Temporal1 TemporalCase1
  | Temporal2 TemporalCase2
  | Compound CompoundGroup CompoundSeries
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Illocution (6 values vs V4's 9)
--------------------------------------------------------------------------------

data Illocution
  = ASR  -- ^ Assertive
  | DIR  -- ^ Directive
  | IRG  -- ^ Interrogative
  | ADM  -- ^ Admonitive
  | HOR  -- ^ Hortative
  | DEC  -- ^ Declarative
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Mood (8 values vs V4's 6)
--------------------------------------------------------------------------------

data Mood
  = FAC  -- ^ Factual
  | SUB  -- ^ Subjunctive
  | ASM  -- ^ Assumptive
  | SPC  -- ^ Speculative
  | COU  -- ^ Counterfactive
  | HYP  -- ^ Hypothetical
  | IPL  -- ^ Implicative (V3-only)
  | ASC  -- ^ Ascriptive (V3-only)
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Valence (shared concept with V4, same 9 values)
--------------------------------------------------------------------------------

data Valence
  = MNO  -- ^ Monoactive
  | PRL  -- ^ Parallel
  | CRO  -- ^ Corollary
  | RCP  -- ^ Reciprocal
  | CPL  -- ^ Complementary
  | DUP  -- ^ Duplicative
  | DEM  -- ^ Demonstrative
  | CNG  -- ^ Contingent
  | PTI  -- ^ Participative
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Phase
--------------------------------------------------------------------------------

data Phase
  = PCT  -- ^ Punctual
  | ITR  -- ^ Iterative
  | REP  -- ^ Repetitive
  | ITM  -- ^ Intermittent
  | RCT  -- ^ Recurrent
  | FRE  -- ^ Frequentative
  | FRG  -- ^ Fragmentative
  | VAC  -- ^ Vacillative
  | FLC  -- ^ Fluctuative
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Sanction (V3-specific, replaced by Validation in V4)
--------------------------------------------------------------------------------

data Sanction
  = PRP'  -- ^ Propositional
  | EPI   -- ^ Epistemological
  | ALG   -- ^ Allegorical
  | IPU   -- ^ Imputative
  | RFU   -- ^ Refutative
  | REB   -- ^ Rebuttative
  | CJT   -- ^ Conjectural (theoretical)
  | EXV   -- ^ Expatiative
  | AXM   -- ^ Axiomatic
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Validation (V3-specific, used in Cg/Cs slot)
--------------------------------------------------------------------------------

data Validation
  = CNF  -- ^ Confirmative
  | AFM  -- ^ Affirmative
  | RPT  -- ^ Reportive
  | INF  -- ^ Inferential
  | ITU  -- ^ Intuitive
  | RVL  -- ^ Revelatory
  | HPT  -- ^ Hypothetical
  | DFT  -- ^ Defective
  | USP  -- ^ Unspecified
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Aspect (V3 uses Cs consonant in Cg/Cs slot)
--------------------------------------------------------------------------------

data Aspect
  = RTR  -- ^ Retrospective
  | PRS  -- ^ Prospective
  | HAB  -- ^ Habitual
  | PRG  -- ^ Progressive
  | IMM  -- ^ Imminent
  | PCS  -- ^ Precessive
  | REG  -- ^ Regressive
  | SMM  -- ^ Summative
  | ATP  -- ^ Anticipatory
  | RSM  -- ^ Resumptive
  | CSS  -- ^ Cessative
  | PAU  -- ^ Pausal
  | RGR  -- ^ Regressive
  | PCL  -- ^ Preclusive
  | CNT  -- ^ Continuative
  | ICS  -- ^ Incessative
  | EXP  -- ^ Experiential
  | IRP  -- ^ Interruptive
  | PMP  -- ^ Preemptive
  | CLM  -- ^ Climactic
  | DLT  -- ^ Dilatory
  | TMP  -- ^ Temporary
  | XPD  -- ^ Expeditive
  | LIM  -- ^ Limitative
  | EPD  -- ^ Expeditious
  | PTC  -- ^ Protractive
  | PPR  -- ^ Preparatory
  | DCL  -- ^ Disclusive
  | CCL  -- ^ Conclusive
  | CUL  -- ^ Culminative
  | IMD  -- ^ Intermediative
  | TRD  -- ^ Tardative
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Level
--------------------------------------------------------------------------------

data Level
  = MIN  -- ^ Minimal
  | SBE  -- ^ Subequative
  | IFR  -- ^ Inferior
  | DFI  -- ^ Deficient
  | EQU  -- ^ Equative
  | SUR  -- ^ Surpassive
  | SPL  -- ^ Superlative
  | SPQ  -- ^ Superequative
  | MAX  -- ^ Maximal
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Bias (49 values, consonant-encoded via Cb)
--------------------------------------------------------------------------------

data Bias
  = NOB             -- ^ Neutral
  | ASU  | ASU_P    -- ^ Assumptive / +
  | HPB  | HPB_P    -- ^ Hyperbolic / +
  | COI  | COI_P    -- ^ Coincidental / +
  | ACP  | ACP_P    -- ^ Acceptive / +
  | RAC  | RAC_P    -- ^ Reactive / +
  | STU  | STU_P    -- ^ Stupefactive / +
  | CTV  | CTV_P    -- ^ Contemplative / +
  | DPV  | DPV_P    -- ^ Desperative / +
  | RVL' | RVL_P    -- ^ Revelative / +
  | GRT  | GRT_P    -- ^ Gratificative / +
  | SOL  | SOL_P    -- ^ Solicitous / +
  | SEL  | SEL_P    -- ^ Selective / +
  | IRO  | IRO_P    -- ^ Ironic / +
  | EXA  | EXA_P    -- ^ Exasperative / +
  | LTL  | LTL_P    -- ^ Literal / +
  | CRR  | CRR_P    -- ^ Corrective / +
  | EUP  | EUP_P    -- ^ Euphemistic / +
  | SKP  | SKP_P    -- ^ Skeptical / +
  | CYN  | CYN_P    -- ^ Cynical / +
  | CTP  | CTP_P    -- ^ Contemptive / +
  | DSM  | DSM_P    -- ^ Dismissive / +
  | IDG  | IDG_P    -- ^ Indignative / +
  | SGS  | SGS_P    -- ^ Suggestive / +
  | PPV  | PPV_P    -- ^ Propositive / +
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Tone (encodes Version)
--------------------------------------------------------------------------------

data Tone
  = Falling        -- ^ PRC (default)
  | High           -- ^ CPT
  | Rising         -- ^ INE
  | Low            -- ^ INC
  | FallingRising  -- ^ PST
  | RisingFalling  -- ^ EFC
  deriving (Show, Eq, Ord, Enum, Bounded)

toneToVersion :: Tone -> Version
toneToVersion Falling       = PRC
toneToVersion High          = CPT
toneToVersion Rising        = INE
toneToVersion Low           = INC
toneToVersion FallingRising = PST
toneToVersion RisingFalling = EFC

versionToTone :: Version -> Tone
versionToTone PRC = Falling
versionToTone CPT = High
versionToTone INE = Rising
versionToTone INC = Low
versionToTone PST = FallingRising
versionToTone EFC = RisingFalling

--------------------------------------------------------------------------------
-- Root
--------------------------------------------------------------------------------

newtype Root = Root Text
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Affix (VxCx derivational suffix)
--------------------------------------------------------------------------------

data Affix = Affix
  { affixVowel     :: Text  -- ^ Vx: degree
  , affixConsonant :: Text  -- ^ Cx: affix type
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Formative Structure
--------------------------------------------------------------------------------

-- | Vr slot: Function + Pattern + Stem
type SlotVr = (Function, Pattern, Stem)

-- | Ca slot: Essence + Extension + Perspective + Affiliation + Configuration
type SlotCa = CaComplex

-- | CiVi slot: Illocution + Mood
type SlotCiVi = (Illocution, Mood)

-- | Vf slot: Context + Format
type SlotVf = (Context, Format)

-- | Complete V3 formative
data Formative = Formative
  { fTone      :: Tone                -- ^ Version (encoded as tone)
  , fVr        :: SlotVr              -- ^ Function + Pattern + Stem
  , fRoot      :: Root                -- ^ Cr consonant root
  , fCase      :: Case                -- ^ Vc case vowel
  , fCiVi      :: Maybe SlotCiVi      -- ^ Illocution + Mood (optional)
  , fCa        :: SlotCa              -- ^ Ca complex
  , fAffixes   :: [Affix]             -- ^ VxCx derivational suffixes
  , fVf        :: Maybe SlotVf        -- ^ Context + Format (optional)
  , fBias      :: Maybe Bias          -- ^ Cb bias (optional)
  , fDesig     :: Designation          -- ^ Stress-encoded
  , fRelation  :: Relation             -- ^ Stress-encoded
  }
  deriving (Show, Eq)

-- | Default/minimal formative
defaultFormative :: Text -> Formative
defaultFormative rootText = Formative
  { fTone     = Falling
  , fVr       = (STA, P1, S1)
  , fRoot     = Root rootText
  , fCase     = Transrelative OBL
  , fCiVi     = Nothing
  , fCa       = defaultCa
  , fAffixes  = []
  , fVf       = Nothing
  , fBias     = Nothing
  , fDesig    = IFL
  , fRelation = Unframed
  }

--------------------------------------------------------------------------------
-- Abbreviation rendering
--------------------------------------------------------------------------------

-- | Render a case as its standard abbreviation
caseAbbrev :: Case -> Text
caseAbbrev (Transrelative c) = case c of
  OBL -> "OBL"; IND -> "IND"; ABS -> "ABS"; ERG -> "ERG"; EFF -> "EFF"
  AFF -> "AFF"; DAT -> "DAT"; INS -> "INS"; ACT -> "ACT"; DER -> "DER"
caseAbbrev (Possessive c) = case c of
  SIT -> "SIT"; POS -> "POS"; PRP -> "PRP"; GEN -> "GEN"; ATT -> "ATT"
  PDC -> "PDC"; ITP -> "ITP"; OGN -> "OGN"; PAR -> "PAR"; CRS -> "CRS"
caseAbbrev (Associative c) = case c of
  CPS -> "CPS"; PRD -> "PRD"; MED -> "MED"; APL -> "APL"; PUR -> "PUR"
  CSD -> "CSD"; ESS -> "ESS"; ASI -> "ASI"; FUN -> "FUN"; TFM -> "TFM"
caseAbbrev (Adverbial c) = case c of
  REF -> "REF"; CLA -> "CLA"; CNV -> "CNV"; IDP -> "IDP"; BEN -> "BEN"
  TSP -> "TSP"; CMM -> "CMM"; COM -> "COM"; CNJ -> "CNJ"; UTL -> "UTL"
caseAbbrev (Relational c) = case c of
  ABE -> "ABE"; CVS -> "CVS"; COR -> "COR"; DEP -> "DEP"; PVS -> "PVS"
  PTL -> "PTL"; CON -> "CON"; EXC -> "EXC"; AVR -> "AVR"; CMP -> "CMP"
caseAbbrev (Temporal1 c) = case c of
  SML -> "SML"; ASS -> "ASS"; CNR -> "CNR"; ACS -> "ACS"; DFF -> "DFF"
  PER -> "PER"; PRO -> "PRO"; PCV -> "PCV"; PCR -> "PCR"; ELP -> "ELP"
  ALP -> "ALP"; INP -> "INP"
caseAbbrev (Temporal2 c) = case c of
  EPS -> "EPS"; PLM -> "PLM"; LMT -> "LMT"; LOC -> "LOC"; ORI -> "ORI"
  PSV -> "PSV"; ALL -> "ALL"; ABL -> "ABL"; NAV -> "NAV"; VOC -> "VOC"
caseAbbrev (Compound g s) =
  let gn = case g of
        Cmp1 -> "1"; Cmp2 -> "2"; Cmp3 -> "3"; Cmp4 -> "4"
        Cmp5 -> "5"; Cmp6 -> "6"; Cmp7 -> "7"; Cmp8 -> "8"
      sn = case s of
        CmpA -> "A"; CmpB -> "B"; CmpC -> "C"
  in "CMP" <> gn <> sn
