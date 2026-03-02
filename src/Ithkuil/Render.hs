{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Rendering
-- Render grammatical structures to text
module Ithkuil.Render where

import Data.Text (Text)
import qualified Data.Text as T
import Ithkuil.Phonology (vowelForm)
import Ithkuil.Grammar
import qualified Ithkuil.Allomorph as Ca
import Ithkuil.Parse (casePatterns, vnTable, cnMoodTable, cnCaseScopeTable)

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
-- Uses the same table as parsing for consistency
renderSlotIV :: SlotIV -> Text
renderSlotIV (func, spec, ctx) =
  vowelForm (contextSeries ctx) (funcSpecForm ctx func spec)
  where
    contextSeries EXS = 1
    contextSeries FNC = 2
    contextSeries RPS = 3
    contextSeries AMG = 4

    -- Series 1 skips form 4, uses form 5 for STA/OBJ
    -- Series 2-4 skip form 5, use form 4 for STA/OBJ
    funcSpecForm EXS STA OBJ = 5
    funcSpecForm _   STA OBJ = 4
    funcSpecForm _   STA BSC = 1
    funcSpecForm _   STA CTE = 2
    funcSpecForm _   STA CSV = 3
    funcSpecForm _   DYN OBJ = 6
    funcSpecForm _   DYN CSV = 7
    funcSpecForm _   DYN CTE = 8
    funcSpecForm _   DYN BSC = 9

-- | Slot V: Stem affixes (CsVx)
renderSlotV :: [Affix] -> Text
renderSlotV = T.concat . map renderAffix

-- | Slot VI: Ca complex (uses allomorphic construction)
renderSlotVI :: SlotVI -> Text
renderSlotVI = Ca.renderCa

-- | Slot VII: CA-scoped affixes (VxCs)
renderSlotVII :: [Affix] -> Text
renderSlotVII = T.concat . map renderAffix

-- | Render single affix
renderAffix :: Affix -> Text
renderAffix a = affixVowel a <> affixConsonant a

-- | Slot VIII: Vn+Cn (Valence + Mood/Case-Scope)
renderSlotVIII :: Maybe (Valence, MoodOrScope) -> Text
renderSlotVIII Nothing = ""
renderSlotVIII (Just (val, ms)) = renderValence val <> renderMoodOrScope ms

renderValence :: Valence -> Text
renderValence v = case lookup v vnTable of
  Just t -> t
  Nothing -> "a"  -- fallback to MNO

renderMoodOrScope :: MoodOrScope -> Text
renderMoodOrScope (MoodVal m) = case lookup m cnMoodTable of
  Just t -> t
  Nothing -> "h"  -- fallback to FAC
renderMoodOrScope (CaseScope cs) = case lookup cs cnCaseScopeTable of
  Just t -> t
  Nothing -> "w"  -- fallback to CCN

-- | Slot IX: Case or Format/Illocution+Validation
renderSlotIX :: Either Case FormatOrIV -> Text
renderSlotIX (Left c) = renderCase c
renderSlotIX (Right Format) = "a"
renderSlotIX (Right (IllocVal ill val)) = renderIllocution ill <> renderValidation val

-- | Render case from lookup table
renderCase :: Case -> Text
renderCase c = case lookup c casePatterns of
  Just t -> t
  Nothing -> "a"  -- fallback to THM

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
renderValidation v = vowelForm 1 (validationForm v)
  where
    validationForm OBS = 1
    validationForm REC = 2
    validationForm PUP = 3
    validationForm RPR = 5
    validationForm USP = 4
    validationForm IMA = 6
    validationForm CVN = 7
    validationForm ITU = 8
    validationForm INF = 9
