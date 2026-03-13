{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Referentials (Personal Reference)
-- Per Chapter 9 of the official grammar
module Ithkuil.Referentials
  ( Referent(..)
  , ReferentEffect(..)
  , PersonalRef(..)
  , refC1
  , refC1All
  , lookupRefC1
  , renderRefCase
  ) where

import Data.Text (Text)
import Data.Maybe (listToMaybe)
import Ithkuil.Grammar (Case(..))
import Ithkuil.Render (renderCase)

--------------------------------------------------------------------------------
-- Referent Categories (per Sec. 9.1)
--------------------------------------------------------------------------------

-- | The 11 referent categories
data Referent
  = R1m   -- ^ Monadic speaker ("I")
  | R2m   -- ^ Monadic addressee ("you sg.")
  | R2p   -- ^ Polyadic addressee ("you pl.")
  | Rma   -- ^ Monadic animate 3rd party ("he/she")
  | Rpa   -- ^ Polyadic animate 3rd party ("they")
  | Rmi   -- ^ Monadic inanimate 3rd party ("it")
  | Rpi   -- ^ Polyadic inanimate 3rd party ("those things")
  | Rmx   -- ^ Mixed animate/inanimate 3rd party
  | Rrdp  -- ^ Reduplicative (resumptive reference)
  | Robv  -- ^ Obviative (other 3rd party)
  | Rpvs  -- ^ Provisional ("whatever")
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Effect applied to referent (determines C1 variant)
data ReferentEffect
  = NEU  -- ^ Neutral
  | BEN  -- ^ Beneficial
  | DET  -- ^ Detrimental
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | A personal reference = referent + effect
data PersonalRef = PersonalRef Referent ReferentEffect
  deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- C1 Consonant Forms (per Sec. 9.1 table)
--------------------------------------------------------------------------------

-- | Get the C1 consonant for a referent+effect combination
refC1 :: PersonalRef -> Text
refC1 (PersonalRef ref eff) = case (ref, eff) of
  -- 1m: monadic speaker "I"
  (R1m, NEU) -> "l"
  (R1m, BEN) -> "r"
  (R1m, DET) -> "ř"
  -- 2m: monadic addressee "you sg."
  (R2m, NEU) -> "s"
  (R2m, BEN) -> "š"
  (R2m, DET) -> "ž"
  -- 2p: polyadic addressee "you pl."
  (R2p, NEU) -> "n"
  (R2p, BEN) -> "t"
  (R2p, DET) -> "d"
  -- ma: monadic animate 3rd party
  (Rma, NEU) -> "m"
  (Rma, BEN) -> "p"
  (Rma, DET) -> "b"
  -- pa: polyadic animate 3rd party
  (Rpa, NEU) -> "ň"
  (Rpa, BEN) -> "k"
  (Rpa, DET) -> "g"
  -- mi: monadic inanimate 3rd party
  (Rmi, NEU) -> "z"
  (Rmi, BEN) -> "ţ"
  (Rmi, DET) -> "ḑ"
  -- pi: polyadic inanimate 3rd party
  (Rpi, NEU) -> "ẓ"
  (Rpi, BEN) -> "f"
  (Rpi, DET) -> "v"
  -- Mx: mixed animate/inanimate
  (Rmx, NEU) -> "c"
  (Rmx, BEN) -> "č"
  (Rmx, DET) -> "j"
  -- Rdp: reduplicative (resumptive)
  (Rrdp, NEU) -> "th"
  (Rrdp, BEN) -> "ph"
  (Rrdp, DET) -> "kh"
  -- Obv: obviative
  (Robv, NEU) -> "ll"
  (Robv, BEN) -> "rr"
  (Robv, DET) -> "řř"
  -- PVS: provisional
  (Rpvs, NEU) -> "mm"
  (Rpvs, BEN) -> "nn"
  (Rpvs, DET) -> "ňň"

-- | All C1 forms as (PersonalRef, Text) pairs for reverse lookup
refC1All :: [(PersonalRef, Text)]
refC1All =
  [ (PersonalRef ref eff, refC1 (PersonalRef ref eff))
  | ref <- [minBound..maxBound]
  , eff <- [minBound..maxBound]
  ]

-- | Look up a C1 consonant to find the referent+effect
lookupRefC1 :: Text -> Maybe PersonalRef
lookupRefC1 c = listToMaybe [pr | (pr, form) <- refC1All, form == c]

--------------------------------------------------------------------------------
-- Referential Case Marking
--------------------------------------------------------------------------------

-- | Render case for referential (uses same Vc vowels as formative Slot IX)
renderRefCase :: Case -> Text
renderRefCase = renderCase
