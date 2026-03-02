{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Adjuncts
-- Bias, Register, Modular, Affixual, and Suppletive adjuncts
module Ithkuil.Adjuncts where

import Data.Text (Text)
import Data.Maybe (listToMaybe)

--------------------------------------------------------------------------------
-- Bias Adjuncts (70+ emotional/attitudinal markers)
--------------------------------------------------------------------------------

data Bias
  -- Dolorous/Melancholic
  = DOL  -- ^ Dolorous: sadness, grief
  | DIS  -- ^ Dismissive: disregard, unimportance
  | DRS  -- ^ Derisive: mockery, ridicule
  | PES  -- ^ Pessimistic: negative expectation
  | DUB  -- ^ Dubitative: doubt, skepticism
  | SKP  -- ^ Skeptical: disbelief
  -- Fearful/Anxious
  | TRP  -- ^ Trepidative: fear, worry
  | APH  -- ^ Apprehensive: concern, unease
  | IPT  -- ^ Impatient: restlessness
  | ANP  -- ^ Anticipative: expectation
  -- Negative Judgment
  | DPB  -- ^ Disapprobative: disapproval
  | CTP  -- ^ Contemptive: contempt
  | IDG  -- ^ Indignative: righteous anger
  | EXA  -- ^ Exasperative: frustration
  | RPU  -- ^ Repulsive: disgust
  | IVD  -- ^ Invidious: envy, resentment
  | VEX  -- ^ Vexative: annoyance
  -- Surprise/Wonder
  | STU  -- ^ Stupefactive: amazement
  | PPX  -- ^ Perplexive: confusion
  | DCC  -- ^ Disconcertive: unsettled
  | RVL  -- ^ Revelative: realization
  | FSC  -- ^ Fascinative: fascination
  -- Positive Emotion
  | EUH  -- ^ Euphoric: elation
  | GRT  -- ^ Gratificative: gratitude
  | SAT  -- ^ Satiative: satisfaction
  | DLC  -- ^ Delectative: delight
  | IFT  -- ^ Infatuative: intense attraction
  -- Social/Relational
  | SOL  -- ^ Solicitative: earnest request
  | RAC  -- ^ Reactive: in response
  | MAN  -- ^ Mandatory: necessity
  | EXG  -- ^ Exigent: urgency
  | ATE  -- ^ Attentive: paying attention
  -- Evaluative
  | APB  -- ^ Approbative: approval
  | OPT  -- ^ Optimal: best outcome
  | CNV  -- ^ Contensive: asserting claim
  | ACC  -- ^ Accidental: unintentional
  | ACH  -- ^ Archetypal: typical example
  -- Speech Act Modifiers
  | IRO  -- ^ Ironic: opposite meaning
  | PSM  -- ^ Presumptive: assuming
  | CRR  -- ^ Corrective: correction
  | EUP  -- ^ Euphemistic: softening
  | PSC  -- ^ Prosaic: matter-of-fact
  | CMD  -- ^ Comedic: humorous
  -- Other
  | PPV  -- ^ Propositive: suggestion
  | SGS  -- ^ Suggestive: implying
  | DFD  -- ^ Diffident: shy, hesitant
  | RFL  -- ^ Reflective: thoughtful
  | DES  -- ^ Desperative: desperation
  | COI  -- ^ Coincidental: by chance
  | FOR  -- ^ Fortuitous: lucky
  | ANN  -- ^ Annunciative: announcing
  | RNC  -- ^ Renunciative: giving up
  | ISP  -- ^ Insipid: bland reaction
  | ADM  -- ^ Admissive: conceding
  | IPL  -- ^ Implicative: implying
  | PPT  -- ^ Propitious: favorable
  | CTV  -- ^ Contemplative: pondering
  | CRP  -- ^ Corruptive: corrupting
  | DEJ  -- ^ Dejective: dejection
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Bias consonant forms
biasForm :: Bias -> Text
biasForm DOL = "řřx"
biasForm DIS = "kff"
biasForm DRS = "pfc"
biasForm PES = "ksp"
biasForm DUB = "mmf"
biasForm SKP = "rnž"
biasForm TRP = "llč"
biasForm APH = "vvz"
biasForm IPT = "žžv"
biasForm ANP = "lst"
biasForm DPB = "ffx"
biasForm CTP = "kšš"
biasForm IDG = "pšš"
biasForm EXA = "kçç"
biasForm RPU = "šštļ"
biasForm IVD = "řřn"
biasForm VEX = "ksk"
biasForm STU = "ļļč"
biasForm PPX = "llh"
biasForm DCC = "gzj"
biasForm RVL = "mmļ"
biasForm FSC = "žžj"
biasForm EUH = "gzz"
biasForm GRT = "mmh"
biasForm SAT = "ļţ"
biasForm DLC = "jmm"
biasForm IFT = "vvr"
biasForm SOL = "ňňs"
biasForm RAC = "kll"
biasForm MAN = "msk"
biasForm EXG = "rrs"
biasForm ATE = "ňj"
biasForm APB = "řs"
biasForm OPT = "ččk"
biasForm CNV = "rrj"
biasForm ACC = "lf"
biasForm ACH = "mçt"
biasForm IRO = "mmž"
biasForm PSM = "nnţ"
biasForm CRR = "ňţ"
biasForm EUP = "vvt"
biasForm PSC = "žžt"
biasForm CMD = "pļļ"
biasForm PPV = "sl"
biasForm SGS = "ltç"
biasForm DFD = "cč"
biasForm RFL = "llm"
biasForm DES = "mřř"
biasForm COI = "ššč"
biasForm FOR = "lzp"
biasForm ANN = "drr"
biasForm RNC = "mzt"
biasForm ISP = "lçp"
biasForm ADM = "lļ"
biasForm IPL = "vll"
biasForm PPT = "mll"
biasForm CTV = "gvv"
biasForm CRP = "gžž"
biasForm DEJ = "žžg"

-- | Parse bias from consonant form
parseBias :: Text -> Maybe Bias
parseBias t = listToMaybe [ b | b <- [minBound..maxBound], biasForm b == t ]

--------------------------------------------------------------------------------
-- Register Adjuncts
--------------------------------------------------------------------------------

data Register
  = NRR  -- ^ Narrative: storytelling
  | DSV  -- ^ Discursive: general discussion
  | PTH  -- ^ Parenthetical: aside
  | CGT  -- ^ Cogitant: internal thought
  | SPF  -- ^ Specificative: clarifying
  | EXM  -- ^ Exemplificative: giving example
  | MTR  -- ^ Mathematical: formal/technical
  | END  -- ^ End of register
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Register consonant forms
registerForm :: Register -> Text
registerForm NRR = "hw"
registerForm DSV = "hlw"
registerForm PTH = "hrw"
registerForm CGT = "hmw"
registerForm SPF = "hnw"
registerForm EXM = "hňw"
registerForm MTR = "hww"  -- geminated
registerForm END = "h"

--------------------------------------------------------------------------------
-- Modular Adjuncts
--------------------------------------------------------------------------------

-- | Modular adjunct: Mood + Case-Scope (or Aspect)
data ModularAdjunct = ModularAdjunct
  { modVn :: Text  -- Valence/Aspect vowel
  , modCn :: Text  -- Mood/Case-Scope consonant
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Affixual Adjuncts
--------------------------------------------------------------------------------

-- | Single affix adjunct
data SingleAffixAdjunct = SingleAffixAdjunct
  { saaVx :: Text    -- Degree vowel
  , saaCs :: Text    -- Affix consonant
  , saaScope :: AffixScope
  }
  deriving (Show, Eq)

-- | Multiple affix adjunct
data MultipleAffixAdjunct = MultipleAffixAdjunct
  { maaAffixes :: [(Text, Text)]  -- (Vx, Cs) pairs
  , maaScope :: AffixScope
  }
  deriving (Show, Eq)

data AffixScope
  = ScopeVII    -- ^ Slot VII (formative-scoped)
  | ScopeV      -- ^ Slot V (stem-scoped)
  | ScopeAdj    -- ^ Adjacent word
  deriving (Show, Eq, Ord, Bounded, Enum)

--------------------------------------------------------------------------------
-- Suppletive Adjuncts
--------------------------------------------------------------------------------

-- | Carrier adjunct (for foreign words, quotes, names)
data CarrierAdjunct = CarrierAdjunct
  { carrierType :: CarrierType
  , carrierCase :: Text  -- Vc
  }
  deriving (Show, Eq)

data CarrierType
  = CarrierQuote       -- ^ Direct quotation
  | CarrierName        -- ^ Proper name
  | CarrierForeign     -- ^ Foreign word
  | CarrierFormula     -- ^ Technical/formula
  deriving (Show, Eq, Ord, Bounded, Enum)

carrierTypeForm :: CarrierType -> Text
carrierTypeForm CarrierQuote   = "hl"
carrierTypeForm CarrierName    = "hm"
carrierTypeForm CarrierForeign = "hn"
carrierTypeForm CarrierFormula = "hň"
