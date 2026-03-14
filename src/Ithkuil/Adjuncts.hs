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
  | ADM  -- ^ Admissive: acknowledgement
  | IPL  -- ^ Implicative: implying
  | EXP  -- ^ Experiential: immediate experience
  | ARB  -- ^ Arbitrary: indifference
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
biasForm DLC = "ẓmm"
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
biasForm RNC = "msf"
biasForm ISP = "lçp"
biasForm ADM = "lļ"
biasForm IPL = "vll"
biasForm PPT = "mll"
biasForm CTV = "gvv"
biasForm CRP = "gžž"
biasForm DEJ = "žžg"
biasForm EXP = "pss"
biasForm ARB = "xtļ"

-- | Parse bias from consonant form
parseBias :: Text -> Maybe Bias
parseBias t = listToMaybe [ b | b <- [minBound..maxBound], biasForm b == t ]

-- | Human-readable bias gloss (representative expression)
biasGloss :: Bias -> Text
biasGloss DOL = "Ow! Ouch!"
biasGloss DIS = "So what!"
biasGloss DRS = "How foolish!"
biasGloss PES = "Pfft!"
biasGloss DUB = "I doubt it"
biasGloss SKP = "Yeah, right!"
biasGloss TRP = "Oh, no!"
biasGloss APH = "I'm worried..."
biasGloss IPT = "C'mon!"
biasGloss ANP = ""
biasGloss DPB = "I don't like that..."
biasGloss CTP = "What nonsense!"
biasGloss IDG = "How dare...!?"
biasGloss EXA = "Don't you get it?"
biasGloss RPU = "Ew! Gross!"
biasGloss IVD = "How unfair!"
biasGloss VEX = "How annoying!"
biasGloss STU = "What the...?"
biasGloss PPX = "Huh?"
biasGloss DCC = "I don't feel comfortable about this..."
biasGloss RVL = "A-ha!"
biasGloss FSC = "Cool! Wow!"
biasGloss EUH = "What bliss!"
biasGloss GRT = "Ahhhh!"
biasGloss SAT = "How satisfying!"
biasGloss DLC = "Whee!"
biasGloss IFT = "Praise be to...!"
biasGloss SOL = "Please"
biasGloss RAC = "My goodness!"
biasGloss MAN = "Take it or leave it"
biasGloss EXG = "It's now or never!"
biasGloss ATE = "Who would have thought?"
biasGloss APB = "OK"
biasGloss OPT = "So!/Totally!"
biasGloss CNV = "I told you so!"
biasGloss ACC = "As luck would have it..."
biasGloss ACH = "Such a...!"
biasGloss IRO = "Just great!"
biasGloss PSM = "It can only mean one thing..."
biasGloss CRR = "What I meant to say is..."
biasGloss EUP = "Let me put it this way..."
biasGloss PSC = "Meh."
biasGloss CMD = "Funny!"
biasGloss PPV = "Consider:"
biasGloss SGS = "How about..."
biasGloss DFD = "It's nothing, just..."
biasGloss RFL = "Look at it this way..."
biasGloss DES = "I'm sorry to have to tell you..."
biasGloss COI = "What a coincidence!"
biasGloss FOR = "All is well that ends well"
biasGloss ANN = "Wait till you hear this!"
biasGloss RNC = "So much for...!"
biasGloss ISP = "How boring!"
biasGloss ADM = "Mm-hm"
biasGloss IPL = "Of course,..."
biasGloss EXP = "Ah! Well, now! So!"
biasGloss ARB = "Yeah, whatever..."
biasGloss PPT = "It's a wonder that..."
biasGloss CTV = "Hmmmm..."
biasGloss CRP = "What corruption!"
biasGloss DEJ = "[dejected sigh]"

--------------------------------------------------------------------------------
-- Register Adjuncts
--------------------------------------------------------------------------------

data Register
  = NRR  -- ^ Narrative (default, unmarked)
  | DSV  -- ^ Discursive: direct speech
  | PNT  -- ^ Parenthetical: aside
  | SPF  -- ^ Specificative: proper name
  | EXM  -- ^ Exemplificative: giving example
  | CGT  -- ^ Cogitant: internal thought
  | END  -- ^ End of register/carrier
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Register adjunct initial forms (h + vowel per Sec. 8.3)
registerForm :: Register -> Text
registerForm NRR = ""     -- default, no marker
registerForm DSV = "ha"
registerForm PNT = "he"
registerForm SPF = "hi"
registerForm EXM = "ho"
registerForm CGT = "hu"
registerForm END = ""    -- END has no initial form, only a final form

-- | Register adjunct final forms (end-of-register markers)
registerFinalForm :: Register -> Text
registerFinalForm DSV = "hai"
registerFinalForm PNT = "hei"
registerFinalForm SPF = "hiu"
registerFinalForm EXM = "hoi"
registerFinalForm CGT = "hui"
registerFinalForm END = "hüi"
registerFinalForm _ = ""

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
  = Carrier            -- ^ General carrier (shortcut for carrier stem -s-)
  | Quotative          -- ^ Quotative (carrier + discursive register)
  | Naming             -- ^ Naming (name referred to as a name)
  | Phrasal            -- ^ Phrasal adjunct (meta-level grammatical info for entire phrase)
  deriving (Show, Eq, Ord, Bounded, Enum)

carrierTypeForm :: CarrierType -> Text
carrierTypeForm Carrier     = "hl"
carrierTypeForm Quotative   = "hm"
carrierTypeForm Naming      = "hn"
carrierTypeForm Phrasal = "hň"
