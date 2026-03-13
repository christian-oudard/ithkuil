{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Morphological Grammar
module Ithkuil.Grammar where

import Data.Text (Text)
import qualified Data.Text as T
import Ithkuil.Phonology (vowelForm)

-- | Helper for bounded enumerations
allOf :: (Enum a, Bounded a) => [a]
allOf = [minBound .. maxBound]

--------------------------------------------------------------------------------
-- Stem and Version (Slot II: Vv)
--------------------------------------------------------------------------------

data Stem = S1 | S2 | S3 | S0
  deriving (Show, Eq, Ord, Enum, Bounded)

data Version
  = PRC  -- ^ Processual: ongoing/incomplete
  | CPT  -- ^ Completive: completed/achieved
  deriving (Show, Eq, Ord, Enum, Bounded)

type SlotII = (Stem, Version)

-- | Encode Stem + Version as Vv vowel
slotIIToVv :: SlotII -> Text
slotIIToVv (S1, PRC) = vowelForm 1 1  -- a
slotIIToVv (S1, CPT) = vowelForm 1 2  -- ä
slotIIToVv (S2, PRC) = vowelForm 1 3  -- e
slotIIToVv (S2, CPT) = vowelForm 1 5  -- i
slotIIToVv (S3, PRC) = vowelForm 1 9  -- u
slotIIToVv (S3, CPT) = vowelForm 1 8  -- ü
slotIIToVv (S0, PRC) = vowelForm 1 7  -- o
slotIIToVv (S0, CPT) = vowelForm 1 6  -- ö

--------------------------------------------------------------------------------
-- Function, Specification, Context (Slot IV: Vr)
--------------------------------------------------------------------------------

data Function
  = STA  -- ^ Stative: state/condition
  | DYN  -- ^ Dynamic: action/event
  deriving (Show, Eq, Ord, Enum, Bounded)

data Specification
  = BSC  -- ^ Basic: holistic/general
  | CTE  -- ^ Contential: content/essence
  | CSV  -- ^ Constitutive: form/makeup
  | OBJ  -- ^ Objective: object/product
  deriving (Show, Eq, Ord, Enum, Bounded)

data Context
  = EXS  -- ^ Existential: neutral/objective
  | FNC  -- ^ Functional: socially/purposeful
  | RPS  -- ^ Representational: subjective/personal
  | AMG  -- ^ Amalgamative: abstract/collective
  deriving (Show, Eq, Ord, Enum, Bounded)

type SlotIV = (Function, Specification, Context)

--------------------------------------------------------------------------------
-- CA Complex (Slot VI): Configuration, Affiliation, Perspective, Extension, Essence
--------------------------------------------------------------------------------

-- | Configuration (20 values)
-- Describes the physical/compositional relationship between set members
data Configuration
  = UNI  -- ^ Uniplex: single entity
  -- Duplex (2 members)
  | DSS  -- ^ Duplex Similar Separate
  | DSC  -- ^ Duplex Similar Connected
  | DSF  -- ^ Duplex Similar Fused
  | DDS  -- ^ Duplex Dissimilar Separate
  | DDC  -- ^ Duplex Dissimilar Connected
  | DDF  -- ^ Duplex Dissimilar Fused
  | DFS  -- ^ Duplex Fuzzy Separate
  | DFC  -- ^ Duplex Fuzzy Connected
  | DFF  -- ^ Duplex Fuzzy Fused
  -- Multiplex (3+ members)
  | MSS  -- ^ Multiplex Similar Separate
  | MSC  -- ^ Multiplex Similar Connected
  | MSF  -- ^ Multiplex Similar Fused
  | MDS  -- ^ Multiplex Dissimilar Separate
  | MDC  -- ^ Multiplex Dissimilar Connected
  | MDF  -- ^ Multiplex Dissimilar Fused
  | MFS  -- ^ Multiplex Fuzzy Separate
  | MFC  -- ^ Multiplex Fuzzy Connected
  | MFF  -- ^ Multiplex Fuzzy Fused
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Affiliation (4 values)
-- Social/functional relationship between set members
data Affiliation
  = CSL  -- ^ Consolidative: members share common purpose
  | ASO  -- ^ Associative: incidental grouping
  | COA  -- ^ Coalescent: complementary/synergistic
  | VAR  -- ^ Variative: conflicting/divergent
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Perspective (4 values)
-- Boundedness and quantification
data Perspective
  = M_   -- ^ Monadic: bounded singular
  | G_   -- ^ Polyadic: bounded plural (renamed to avoid conflict)
  | N_   -- ^ Nomic: generic/timeless
  | A_   -- ^ Abstract: conceptual/platonic
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Extension (6 values)
-- Temporal/spatial extent
data Extension
  = DEL  -- ^ Delimitive: whole/complete
  | PRX  -- ^ Proximal: initial portion
  | ICP  -- ^ Incipient: beginning
  | ATV  -- ^ Attenuative: diminishing
  | GRA  -- ^ Graduative: gradual
  | DPL  -- ^ Depletive: final portion
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Essence (2 values)
-- Real vs hypothetical
data Essence
  = NRM  -- ^ Normal: real/actual
  | RPV  -- ^ Representative: hypothetical/symbolic
  deriving (Show, Eq, Ord, Enum, Bounded)

type SlotVI = (Configuration, Affiliation, Perspective, Extension, Essence)

--------------------------------------------------------------------------------
-- Case (68 cases in 8 groups)
--------------------------------------------------------------------------------

-- | Transrelative Cases (9) - core argument roles
data TransrelativeCase
  = THM  -- ^ Thematic: content/theme
  | INS  -- ^ Instrumental: means/instrument
  | ABS  -- ^ Absolutive: patient of dynamic
  | AFF  -- ^ Affective: experiencer
  | STM  -- ^ Stimulative: stimulus
  | EFF  -- ^ Effectuative: enabler
  | ERG  -- ^ Ergative: agent
  | DAT  -- ^ Dative: recipient
  | IND  -- ^ Inducive: causer
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Appositive Cases (9) - possessive/attributive
data AppositiveCase
  = POS  -- ^ Possessive
  | PRP  -- ^ Proprietive: ownership
  | GEN  -- ^ Genitive: source/origin
  | ATT  -- ^ Attributive: quality
  | PDC  -- ^ Productive: creator
  | ITP  -- ^ Interpretive: perspective
  | OGN  -- ^ Originative: original source
  | IDP  -- ^ Interdependent
  | PAR  -- ^ Partitive: part of whole
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Associative Cases (9) - relationships
data AssociativeCase
  = APL  -- ^ Applicative: purpose
  | PUR  -- ^ Purposive: goal
  | TRA  -- ^ Transmissive: means of transmission
  | DFR  -- ^ Deferential: on behalf of
  | CRS  -- ^ Contrastive: contrast
  | TSP  -- ^ Transpositive: exchange
  | CMM  -- ^ Commutative: in exchange for
  | CMP  -- ^ Comparative: comparison
  | CSD  -- ^ Considerative: in terms of
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Adverbial Cases (9) - circumstantial
data AdverbialCase
  = FUN  -- ^ Functive: manner
  | TFM  -- ^ Transformative: becoming
  | CLA  -- ^ Classificative: as member of
  | RSL  -- ^ Resultative: resulting in
  | CSM  -- ^ Consumptive: using up
  | CON  -- ^ Concessive: despite
  | AVR  -- ^ Aversive: avoiding
  | CVS  -- ^ Conversive: in reversal of
  | SIT  -- ^ Situative: in situation of
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Relational Cases (8) - comparison/correlation
data RelationalCase
  = PRN  -- ^ Pertinential: regarding
  | DSP  -- ^ Descriptive: described as
  | COR  -- ^ Correlative: correlated with
  | CPS  -- ^ Compositive: composed of
  | COM  -- ^ Comitative: together with
  | UTL  -- ^ Utilitative: using
  | PRD  -- ^ Predicative: being
  | RLT  -- ^ Relative: which/that
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Affinitive Cases (8) - attitude/relationship
data AffinitiveCase
  = ACT  -- ^ Activative: actively engaged
  | ASI  -- ^ Assimilative: similar to
  | ESS  -- ^ Essive: in role of
  | TRM  -- ^ Terminative: up until
  | SEL  -- ^ Selective: among choices
  | CFM  -- ^ Conformative: according to
  | DEP  -- ^ Dependent: depending on
  | VOC  -- ^ Vocative: addressing
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Spatio-Temporal I Cases (8) - location/direction
data SpatioTemporal1Case
  = LOC  -- ^ Locative: at/in
  | ATD  -- ^ Attendant: in presence of
  | ALL  -- ^ Allative: toward
  | ABL  -- ^ Ablative: from
  | ORI  -- ^ Orientative: facing
  | IRL  -- ^ Interrelative: between
  | INV  -- ^ Intrative: into
  | NAV  -- ^ Navigative: via/through
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Spatio-Temporal II Cases (8) - more location/time
data SpatioTemporal2Case
  = CNR  -- ^ Concursive: at same time
  | ASS  -- ^ Assessive: to extent of
  | PER  -- ^ Periodic: recurring
  | PRO  -- ^ Prolapsive: for duration
  | PCV  -- ^ Precursive: before
  | PCR  -- ^ Postcursive: after
  | ELP  -- ^ Elapsive: since
  | PLM  -- ^ Prolimitive: until
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | All 68 cases unified
data Case
  = Transrelative TransrelativeCase
  | Appositive AppositiveCase
  | Associative AssociativeCase
  | Adverbial AdverbialCase
  | Relational RelationalCase
  | Affinitive AffinitiveCase
  | SpatioTemporal1 SpatioTemporal1Case
  | SpatioTemporal2 SpatioTemporal2Case
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Valence, Phase, Effect, Aspect (Verbal Categories)
--------------------------------------------------------------------------------

-- | Valence (9 values) - argument structure
data Valence
  = MNO  -- ^ Monoactive: single argument
  | PRL  -- ^ Parallel: both affected equally
  | CRO  -- ^ Corollary: cause-effect
  | RCP  -- ^ Reciprocal: mutual action
  | CPL  -- ^ Complementary: different contributions
  | DUP  -- ^ Duplicative: repeated action
  | DEM  -- ^ Demonstrative: as example
  | CNG  -- ^ Contingent: depending on
  | PTI  -- ^ Participative: sharing in
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Phase (9 values) - temporal contour of action
data Phase
  = PUN  -- ^ Punctual: instantaneous
  | ITR  -- ^ Iterative: repeated
  | REP  -- ^ Repetitive: recurring
  | ITM  -- ^ Intermittent: on-and-off
  | RCT  -- ^ Recurrent: habitual
  | FRE  -- ^ Frequentative: often
  | FRG  -- ^ Fragmentative: in pieces
  | VAC  -- ^ Vacillative: back and forth
  | FLC  -- ^ Fluctuative: varying
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Effect (4 values) - speaker's benefit assessment
data Effect
  = BEN1 -- ^ Beneficial to speaker
  | BEN2 -- ^ Beneficial to addressee
  | BEN3 -- ^ Beneficial to third party
  | BENX -- ^ Beneficial unknown/general
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Aspect (36 values) - temporal phases
data Aspect
  = RTR  -- ^ Retrospective: looking back
  | PRS  -- ^ Prospective: looking forward
  | HAB  -- ^ Habitual
  | PRG  -- ^ Progressive: ongoing
  | IMM  -- ^ Imminent: about to
  | PCS  -- ^ Precessive: just before
  | REG  -- ^ Regressive: returning to
  | SMM  -- ^ Summative: overall
  | ATP  -- ^ Anticipatory
  | RSM  -- ^ Resumptive: resuming
  | CSS  -- ^ Cessative: stopping
  | PAU  -- ^ Pausal: pausing
  | RGR  -- ^ Regressive
  | PCL  -- ^ Preclusive: before
  | CNT  -- ^ Continuative: still
  | ICS  -- ^ Incessative: increasingly
  | EXP  -- ^ Experiential: having experienced
  | IRP  -- ^ Interruptive
  | PMP  -- ^ Preemptive
  | CLM  -- ^ Climactic
  | DLT  -- ^ Dilatory: delayed
  | TMP  -- ^ Temporary
  | XPD  -- ^ Expeditive: quickly
  | LIM  -- ^ Limitative: limited
  | EPD  -- ^ Expeditious: speedily
  | PTC  -- ^ Protractive: prolonged
  | PPR  -- ^ Preparatory
  | DCL  -- ^ Disclusive
  | CCL  -- ^ Conclusive
  | CUL  -- ^ Culminative
  | IMD  -- ^ Intermediative
  | TRD  -- ^ Tardative: slowly
  | TNS  -- ^ Transitional
  | ITC  -- ^ Intercommutative
  | MTV  -- ^ Motive
  | SQN  -- ^ Sequential
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Illocution, Validation, Mood
--------------------------------------------------------------------------------

-- | Illocution (9 values) - speech act type
data Illocution
  = ASR  -- ^ Assertive: stating fact
  | DIR  -- ^ Directive: command
  | DEC  -- ^ Declarative: making it so
  | IRG  -- ^ Interrogative: question
  | VER  -- ^ Verificative: confirming
  | ADM  -- ^ Admonitive: warning
  | POT  -- ^ Potentiative: possibility
  | HOR  -- ^ Hortative: encouraging
  | CNJ  -- ^ Conjectural: speculating
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Validation (9 values) - evidentiality
data Validation
  = OBS  -- ^ Observational: directly observed
  | REC  -- ^ Recollective: remembered
  | PUP  -- ^ Purportive: hearsay
  | RPR  -- ^ Reportive: reported
  | USP  -- ^ Unspecified
  | IMA  -- ^ Imaginary: imagined
  | CVN  -- ^ Conventional: commonly believed
  | ITU  -- ^ Intuitive: intuited
  | INF  -- ^ Inferential: inferred
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Mood (9 values) - reality status
data Mood
  = FAC  -- ^ Factual
  | SUB  -- ^ Subjunctive: hypothetical
  | ASM  -- ^ Assumptive: assumed
  | SPC  -- ^ Speculative
  | COU  -- ^ Counterfactive: contrary to fact
  | HYP  -- ^ Hypothetical
  -- Note: More moods exist in full spec
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Formative Structure (10 slots)
--------------------------------------------------------------------------------

-- | Root consonant cluster
newtype Root = Root Text
  deriving (Show, Eq, Ord)

-- | Complete formative structure
data Formative = Formative
  { fSlotI    :: Maybe ConcatenationStatus  -- Cc
  , fSlotII   :: SlotII                      -- Vv (Stem + Version)
  , fSlotIII  :: Root                        -- Cr (Root)
  , fSlotIV   :: SlotIV                      -- Vr (Function + Spec + Context)
  , fSlotV    :: [Affix]                     -- CsVx (Stem affixes)
  , fSlotVI   :: SlotVI                      -- Ca (Configuration complex)
  , fSlotVII  :: [Affix]                     -- VxCs (CA-scoped affixes)
  , fSlotVIII :: Maybe SlotVIII                -- VnCn
  , fSlotIX   :: Either Case FormatOrIV      -- Vc/Vk
  , fStress   :: Stress                      -- Determines Slot IX type
  }
  deriving (Show, Eq)

data ConcatenationStatus = Type1 | Type2
  deriving (Show, Eq, Ord, Enum, Bounded)

data Stress = Penultimate | Ultimate | Antepenultimate
  deriving (Show, Eq, Ord, Enum, Bounded)

data MoodOrScope = MoodVal Mood | CaseScope CaseScope
  deriving (Show, Eq, Ord)

data CaseScope = CCN | CCA | CCS | CCQ | CCP | CCV
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Slot VIII: VnCn affix (Pattern 1 or Pattern 2)
data SlotVIII
  = VnCnValence Valence MoodOrScope    -- ^ Pattern 1: Valence + Mood/CaseScope
  | VnCnPhase Phase MoodOrScope        -- ^ Pattern 1: Phase + Mood/CaseScope
  | VnCnAspect Aspect MoodOrScope      -- ^ Pattern 2: Aspect + Mood/CaseScope
  deriving (Show, Eq, Ord)

data FormatOrIV = Format | IllocVal Illocution Validation
  deriving (Show, Eq, Ord)

-- | Affix: VXCS slot
data Affix = Affix
  { affixVowel :: Text      -- Degree (1-9)
  , affixConsonant :: Text  -- Affix type
  , affixType :: AffixType
  }
  deriving (Show, Eq)

data AffixType = Type1Affix | Type2Affix | Type3Affix
  deriving (Show, Eq, Ord, Enum, Bounded)

--------------------------------------------------------------------------------
-- Default/Empty values
--------------------------------------------------------------------------------

defaultSlotII :: SlotII
defaultSlotII = (S1, PRC)

defaultSlotIV :: SlotIV
defaultSlotIV = (STA, BSC, EXS)

defaultSlotVI :: SlotVI
defaultSlotVI = (UNI, CSL, M_, DEL, NRM)

-- | Create a minimal formative from just a root
minimalFormative :: Text -> Formative
minimalFormative rootText = Formative
  { fSlotI    = Nothing
  , fSlotII   = defaultSlotII
  , fSlotIII  = Root rootText
  , fSlotIV   = defaultSlotIV
  , fSlotV    = []
  , fSlotVI   = defaultSlotVI
  , fSlotVII  = []
  , fSlotVIII = Nothing
  , fSlotIX   = Left (Transrelative THM)
  , fStress   = Penultimate
  }
