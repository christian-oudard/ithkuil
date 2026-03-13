{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Rendering
-- Render grammatical structures to text
module Ithkuil.Render where

import Data.Text (Text)
import qualified Data.Text as T
import Ithkuil.Phonology
import Ithkuil.Grammar

-- | Render a complete formative to text
renderFormative :: Formative -> Text
renderFormative f = T.concat
  [ renderSlotI (fSlotI f)
  , slotIIToVv (fSlotII f)
  , renderRoot (fSlotIII f)
  , renderSlotIV (fSlotIV f)
  , renderSlotV (fSlotV f)
  , renderSlotVI (fSlotVI f)
  , renderSlotVII (fSlotVII f)
  , renderSlotVIII (fSlotVIII f)
  , renderSlotIX (fSlotIX f)
  ]

-- | Slot I: Concatenation status
renderSlotI :: Maybe ConcatenationStatus -> Text
renderSlotI Nothing = ""
renderSlotI (Just Type1) = "h"
renderSlotI (Just Type2) = "hw"

-- | Slot III: Root
renderRoot :: Root -> Text
renderRoot (Root r) = r

-- | Slot IV: Vr (Function + Specification + Context)
renderSlotIV :: SlotIV -> Text
renderSlotIV (func, spec, ctx) =
  vowelForm (contextSeries ctx) (funcSpecForm func spec)
  where
    contextSeries EXS = 1
    contextSeries FNC = 2
    contextSeries RPS = 3
    contextSeries AMG = 4

    funcSpecForm STA BSC = 1
    funcSpecForm STA CTE = 2
    funcSpecForm STA CSV = 3
    funcSpecForm STA OBJ = 5
    funcSpecForm DYN BSC = 9
    funcSpecForm DYN CTE = 8
    funcSpecForm DYN CSV = 7
    funcSpecForm DYN OBJ = 6

-- | Slot V: Stem affixes (CsVx)
renderSlotV :: [Affix] -> Text
renderSlotV = T.concat . map renderAffix

-- | Slot VI: Ca complex
renderSlotVI :: SlotVI -> Text
renderSlotVI (cfg, aff, per, ext, ess) = T.concat
  [ renderConfiguration cfg
  , renderExtension ext
  , renderAffiliation aff
  , renderPerspectiveEssence per ess
  ]

-- | Configuration consonant (Ca1)
-- From official grammar ch03: UPX=null, DPX=s, MSS=t, MSC=k, etc.
renderConfiguration :: Configuration -> Text
renderConfiguration UNI = ""
renderConfiguration DSS = "c"
renderConfiguration DSC = "ks"
renderConfiguration DSF = "ps"
renderConfiguration DDS = "ţs"
renderConfiguration DDC = "fs"
renderConfiguration DDF = "š"
renderConfiguration DFS = "č"
renderConfiguration DFC = "kš"
renderConfiguration DFF = "pš"
renderConfiguration MSS = "t"
renderConfiguration MSC = "k"
renderConfiguration MSF = "p"
renderConfiguration MDS = "ţ"
renderConfiguration MDC = "f"
renderConfiguration MDF = "ç"
renderConfiguration MFS = "z"
renderConfiguration MFC = "ž"
renderConfiguration MFF = "ẓ"

-- | Extension consonant (Ca2)
-- After non-UPX config: voiceless stops; standalone UPX: voiced stops
renderExtension :: Extension -> Text
renderExtension DEL = ""
renderExtension PRX = "t"
renderExtension ICP = "k"
renderExtension ATV = "p"
renderExtension GRA = "g"
renderExtension DPL = "b"

-- | Affiliation consonant (Ca3)
renderAffiliation :: Affiliation -> Text
renderAffiliation CSL = ""
renderAffiliation ASO = "l"
renderAffiliation COA = "r"
renderAffiliation VAR = "ř"

-- | Perspective + Essence (Ca4)
-- After consonant: use "after" forms; standalone: use "standalone" forms
renderPerspectiveEssence :: Perspective -> Essence -> Text
renderPerspectiveEssence M_ NRM = "l"    -- standalone; after consonant = null
renderPerspectiveEssence G_ NRM = "r"
renderPerspectiveEssence N_ NRM = "w"
renderPerspectiveEssence A_ NRM = "y"
renderPerspectiveEssence M_ RPV = "ř"
renderPerspectiveEssence G_ RPV = "l"
renderPerspectiveEssence N_ RPV = "m"
renderPerspectiveEssence A_ RPV = "n"

-- | Slot VII: CA-scoped affixes (VxCs)
renderSlotVII :: [Affix] -> Text
renderSlotVII = T.concat . map renderAffix

-- | Render single affix
renderAffix :: Affix -> Text
renderAffix a = affixVowel a <> affixConsonant a

-- | Slot VIII: VnCn (Pattern 1: Valence/Phase, Pattern 2: Aspect)
renderSlotVIII :: Maybe SlotVIII -> Text
renderSlotVIII Nothing = ""
renderSlotVIII (Just (VnCnValence val ms)) = renderValence val <> renderMoodOrScope ms
renderSlotVIII (Just (VnCnPhase ph ms)) = renderPhase ph <> renderMoodOrScope ms
renderSlotVIII (Just (VnCnAspect asp ms)) = renderAspect asp <> renderMoodOrScopeP2 ms

renderValence :: Valence -> Text
renderValence MNO = "a"
renderValence PRL = "ä"
renderValence CRO = "e"
renderValence RCP = "i"
renderValence CPL = "ëi"
renderValence DUP = "ö"
renderValence DEM = "o"
renderValence CNG = "ü"
renderValence PTI = "u"

-- | Pattern 1 Cn consonants (Mood + Case-Scope)
renderMoodOrScope :: MoodOrScope -> Text
renderMoodOrScope (MoodVal FAC) = "h"
renderMoodOrScope (MoodVal SUB) = "hl"
renderMoodOrScope (MoodVal ASM) = "hr"
renderMoodOrScope (MoodVal SPC) = "hm"
renderMoodOrScope (MoodVal COU) = "hn"
renderMoodOrScope (MoodVal HYP) = "hň"
renderMoodOrScope (CaseScope CCN) = "h"
renderMoodOrScope (CaseScope CCA) = "hl"
renderMoodOrScope (CaseScope CCS) = "hr"
renderMoodOrScope (CaseScope CCQ) = "hm"
renderMoodOrScope (CaseScope CCP) = "hn"
renderMoodOrScope (CaseScope CCV) = "hň"

-- | Pattern 2 Cn consonants (for Aspect)
renderMoodOrScopeP2 :: MoodOrScope -> Text
renderMoodOrScopeP2 (MoodVal FAC) = "w"
renderMoodOrScopeP2 (MoodVal SUB) = "hw"
renderMoodOrScopeP2 (MoodVal ASM) = "hrw"
renderMoodOrScopeP2 (MoodVal SPC) = "hmw"
renderMoodOrScopeP2 (MoodVal COU) = "hnw"
renderMoodOrScopeP2 (MoodVal HYP) = "hňw"
renderMoodOrScopeP2 (CaseScope CCN) = "w"
renderMoodOrScopeP2 (CaseScope CCA) = "hw"
renderMoodOrScopeP2 (CaseScope CCS) = "hrw"
renderMoodOrScopeP2 (CaseScope CCQ) = "hmw"
renderMoodOrScopeP2 (CaseScope CCP) = "hnw"
renderMoodOrScopeP2 (CaseScope CCV) = "hňw"

-- | Render Phase vowels (Series 2)
renderPhase :: Phase -> Text
renderPhase PUN = "ai"
renderPhase ITR = "au"
renderPhase REP = "ei"
renderPhase ITM = "eu"
renderPhase RCT = "ëu"
renderPhase FRE = "ou"
renderPhase FRG = "oi"
renderPhase VAC = "iu"
renderPhase FLC = "ui"

-- | Render Aspect vowels (all 4 series)
renderAspect :: Aspect -> Text
-- Column 1 (Series 1)
renderAspect RTR = "a"
renderAspect PRS = "ä"
renderAspect HAB = "e"
renderAspect PRG = "i"
renderAspect IMM = "ëi"
renderAspect PCS = "ö"
renderAspect REG = "o"
renderAspect SMM = "ü"
renderAspect ATP = "u"
-- Column 2 (Series 2)
renderAspect RSM = "ai"
renderAspect CSS = "au"
renderAspect PAU = "ei"
renderAspect RGR = "eu"
renderAspect PCL = "ëu"
renderAspect CNT = "ou"
renderAspect ICS = "oi"
renderAspect EXP = "iu"
renderAspect IRP = "ui"
-- Column 3 (Series 3)
renderAspect PMP = "ia"
renderAspect CLM = "ie"
renderAspect DLT = "io"
renderAspect TMP = "iö"
renderAspect XPD = "eë"
renderAspect LIM = "uö"
renderAspect EPD = "uo"
renderAspect PTC = "ue"
renderAspect PPR = "ua"
-- Column 4 (Series 4)
renderAspect DCL = "ao"
renderAspect CCL = "aö"
renderAspect CUL = "eo"
renderAspect IMD = "eö"
renderAspect TRD = "oë"
renderAspect TNS = "öe"
renderAspect ITC = "oe"
renderAspect MTV = "öa"
renderAspect SQN = "oa"

-- | Slot IX: Case or Format/Illocution+Validation
renderSlotIX :: Either Case FormatOrIV -> Text
renderSlotIX (Left c) = renderCase c
renderSlotIX (Right Format) = "a"
renderSlotIX (Right (IllocVal ill val)) = renderIllocution ill <> renderValidation val

renderCase :: Case -> Text
-- Transrelative (Series 1)
renderCase (Transrelative THM) = "a"
renderCase (Transrelative INS) = "ä"
renderCase (Transrelative ABS) = "e"
renderCase (Transrelative AFF) = "i"
renderCase (Transrelative STM) = "ëi"
renderCase (Transrelative EFF) = "ö"
renderCase (Transrelative ERG) = "o"
renderCase (Transrelative DAT) = "ü"
renderCase (Transrelative IND) = "u"
-- Appositive (Series 2)
renderCase (Appositive POS) = "ai"
renderCase (Appositive PRP) = "au"
renderCase (Appositive GEN) = "ei"
renderCase (Appositive ATT) = "eu"
renderCase (Appositive PDC) = "ëu"
renderCase (Appositive ITP) = "ou"
renderCase (Appositive OGN) = "oi"
renderCase (Appositive IDP) = "iu"
renderCase (Appositive PAR) = "ui"
-- Associative (Series 3)
renderCase (Associative APL) = "ia"
renderCase (Associative PUR) = "iä"
renderCase (Associative TRA) = "ie"
renderCase (Associative DFR) = "ië"
renderCase (Associative CRS) = "ëo"
renderCase (Associative TSP) = "iö"
renderCase (Associative CMM) = "io"
renderCase (Associative CMP) = "iü"
renderCase (Associative CSD) = "iu"
-- Adverbial (Series 4 - from casePatterns)
renderCase (Adverbial FUN) = "ua"
renderCase (Adverbial TFM) = "uä"
renderCase (Adverbial CLA) = "ue"
renderCase (Adverbial RSL) = "uë"
renderCase (Adverbial CSM) = "ëa"
renderCase (Adverbial CON) = "uö"
renderCase (Adverbial AVR) = "uo"
renderCase (Adverbial CVS) = "uü"
renderCase (Adverbial SIT) = "ui"
-- Relational (Series 5)
renderCase (Relational PRN) = "ao"
renderCase (Relational DSP) = "aö"
renderCase (Relational COR) = "eo"
renderCase (Relational CPS) = "eö"
renderCase (Relational COM) = "oë"
renderCase (Relational UTL) = "öe"
renderCase (Relational PRD) = "oe"
renderCase (Relational RLT) = "öa"
-- Affinitive (Series 6)
renderCase (Affinitive ACT) = "oa"
renderCase (Affinitive ASI) = "öä"
renderCase (Affinitive ESS) = "ea"
renderCase (Affinitive TRM) = "eä"
renderCase (Affinitive SEL) = "ëë"
renderCase (Affinitive CFM) = "öö"
renderCase (Affinitive DEP) = "ëö"
renderCase (Affinitive VOC) = "öë"
-- Spatio-Temporal I (Series 7)
renderCase (SpatioTemporal1 LOC) = "a'a"
renderCase (SpatioTemporal1 ATD) = "ä'ä"
renderCase (SpatioTemporal1 ALL) = "e'e"
renderCase (SpatioTemporal1 ABL) = "ë'ë"
renderCase (SpatioTemporal1 ORI) = "ë'a"
renderCase (SpatioTemporal1 IRL) = "ö'ö"
renderCase (SpatioTemporal1 INV) = "o'o"
renderCase (SpatioTemporal1 NAV) = "ü'ü"
-- Spatio-Temporal II (Series 8)
renderCase (SpatioTemporal2 CNR) = "i'i"
renderCase (SpatioTemporal2 ASS) = "u'u"
renderCase (SpatioTemporal2 PER) = "a'i"
renderCase (SpatioTemporal2 PRO) = "i'a"
renderCase (SpatioTemporal2 PCV) = "e'i"
renderCase (SpatioTemporal2 PCR) = "i'e"
renderCase (SpatioTemporal2 ELP) = "u'a"
renderCase (SpatioTemporal2 PLM) = "a'u"

renderIllocution :: Illocution -> Text
renderIllocution ASR = ""
renderIllocution DIR = "h"
renderIllocution DEC = "hl"
renderIllocution IRG = "hr"
renderIllocution VER = "hw"
renderIllocution ADM = "hm"
renderIllocution POT = "hn"
renderIllocution HOR = "hň"
renderIllocution CNJ = ""

renderValidation :: Validation -> Text
renderValidation OBS = "a"
renderValidation REC = "ä"
renderValidation PUP = "e"
renderValidation RPR = "i"
renderValidation USP = "ëi"
renderValidation IMA = "ö"
renderValidation CVN = "o"
renderValidation ITU = "ü"
renderValidation INF = "u"
