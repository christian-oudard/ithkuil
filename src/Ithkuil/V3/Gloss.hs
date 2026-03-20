{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V3 (Elartkha) Glossing
-- Produces human-readable morphological glosses from parsed formatives.
module Ithkuil.V3.Gloss
  ( glossFormative
  , glossSlots
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Ithkuil.V3.Grammar

-- | Gloss a V3 formative as a single-line morphological breakdown
glossFormative :: Formative -> Text
glossFormative f = T.intercalate "-" $ filter (not . T.null)
  [ glossVr (fVr f)
  , "'" <> rootText (fRoot f) <> "'"
  , caseAbbrev (fCase f)
  , maybe "" glossCiVi (fCiVi f)
  , glossCa (fCa f)
  , maybe "" glossVf (fVf f)
  , maybe "" (T.pack . show) (fBias f)
  , glossDesigRel (fDesig f) (fRelation f)
  , glossVersion (toneToVersion (fTone f))
  ]
  where
    rootText (Root t) = t

-- | Gloss a formative as slot-by-slot breakdown
glossSlots :: Formative -> [(Text, Text)]
glossSlots f = filter ((/= "") . snd)
  [ ("Tone",    glossVersion (toneToVersion (fTone f)))
  , ("Vr",      glossVr (fVr f))
  , ("Cr",      let Root t = fRoot f in t)
  , ("Vc",      caseAbbrev (fCase f))
  , ("Ci+Vi",   maybe "" glossCiVi (fCiVi f))
  , ("Ca",      glossCa (fCa f))
  , ("Vf",      maybe "" glossVf (fVf f))
  , ("Cb",      maybe "" (T.pack . show) (fBias f))
  , ("Stress",  glossDesigRel (fDesig f) (fRelation f))
  ]

--------------------------------------------------------------------------------
-- Slot glossing helpers
--------------------------------------------------------------------------------

glossVr :: SlotVr -> Text
glossVr (func, pat, stem) =
  T.pack (show func) <> "/" <> T.pack (show pat) <> "/" <> T.pack (show stem)

glossCiVi :: SlotCiVi -> Text
glossCiVi (ill, mood) = T.pack (show ill) <> "/" <> T.pack (show mood)

glossCa :: CaComplex -> Text
glossCa (ess, ext, per, aff, cfg)
  | ca == defaultCa = ""  -- Don't show default
  | otherwise = T.intercalate "." $ filter (/= "") parts
  where
    ca = (ess, ext, per, aff, cfg)
    parts =
      [ if cfg /= UNI then T.pack (show cfg) else ""
      , if aff /= CSL then T.pack (show aff) else ""
      , if per /= M_  then showPer per else ""
      , if ext /= DEL then T.pack (show ext) else ""
      , if ess /= NRM then T.pack (show ess) else ""
      ]
    showPer M_ = "M"
    showPer U_ = "U"
    showPer N_ = "N"
    showPer A_ = "A"

glossVf :: SlotVf -> Text
glossVf (ctx, fmt)
  | ctx == EXS && fmt == NOF = ""
  | otherwise = ctxPart <> fmtPart
  where
    ctxPart = if ctx /= EXS then T.pack (show ctx) <> "/" else ""
    fmtPart = if fmt /= NOF then T.pack (show fmt) else ""

glossVersion :: Version -> Text
glossVersion PRC = ""  -- Default, don't show
glossVersion v   = T.pack (show v)

glossDesigRel :: Designation -> Relation -> Text
glossDesigRel IFL Unframed = ""  -- Default
glossDesigRel d r = dPart <> rPart
  where
    dPart = case d of { FML -> "FML"; IFL -> "" }
    rPart = case r of { Framed -> "/FRM"; Unframed -> "" }
