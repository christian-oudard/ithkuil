{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Referentials (Personal Reference)
-- Pronouns and deictic markers
module Ithkuil.Referentials where

import Data.Text (Text)
import qualified Data.Text as T
import Ithkuil.Grammar (Case(..))

--------------------------------------------------------------------------------
-- Personal Reference Categories
--------------------------------------------------------------------------------

-- | Speaker/Addressee reference
data PersonalRef
  = M1   -- ^ Monadic 1st person (I)
  | M2   -- ^ Monadic 2nd person (you singular)
  | MA   -- ^ Monadic Animate 3rd person (he/she)
  | MI   -- ^ Monadic Inanimate 3rd person (it)
  | MX   -- ^ Monadic Mixed 3rd person (they singular)
  | MObv -- ^ Monadic Obviative (that other one)
  | MPvs -- ^ Monadic Provisional (one, someone)
  -- Polyadic forms
  | P1   -- ^ Polyadic 1st person (we exclusive)
  | P2   -- ^ Polyadic 2nd person (you plural)
  | PA   -- ^ Polyadic Animate (they animate)
  | PI   -- ^ Polyadic Inanimate (they inanimate)
  | PX   -- ^ Polyadic Mixed (they mixed)
  | PObv -- ^ Polyadic Obviative
  | PPvs -- ^ Polyadic Provisional (some people)
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Referential C1 forms (initial consonant)
refC1 :: PersonalRef -> Text
refC1 M1   = "l"
refC1 M2   = "s"
refC1 MA   = "n"
refC1 MI   = "t"
refC1 MX   = "k"
refC1 MObv = "x"
refC1 MPvs = "ç"
refC1 P1   = "m"
refC1 P2   = "z"
refC1 PA   = "ň"
refC1 PI   = "d"
refC1 PX   = "g"
refC1 PObv = "ř"
refC1 PPvs = "j"

--------------------------------------------------------------------------------
-- Dual Referentials (two referents)
--------------------------------------------------------------------------------

-- | Dual referential: two referents in one word
data DualRef = DualRef PersonalRef PersonalRef
  deriving (Show, Eq)

-- | Render dual referential
renderDualRef :: DualRef -> Case -> Text
renderDualRef (DualRef r1 r2) c =
  refC1 r1 <> refC1 r2 <> renderRefCase c

--------------------------------------------------------------------------------
-- Combination Referentials
--------------------------------------------------------------------------------

-- | Combination: 1st + 2nd person
data CombinationRef
  = C1Plus2m  -- ^ I + you (singular)
  | C1Plus2p  -- ^ I + you (plural)
  | CWeIncl   -- ^ We (inclusive: I + you + maybe others)
  deriving (Show, Eq, Ord, Bounded, Enum)

combRefForm :: CombinationRef -> Text
combRefForm C1Plus2m = "tļ"
combRefForm C1Plus2p = "tļw"
combRefForm CWeIncl  = "tļm"

--------------------------------------------------------------------------------
-- Referential Case Marking
--------------------------------------------------------------------------------

-- | Render case for referential (Vc2)
renderRefCase :: Case -> Text
renderRefCase (Transrelative c) = transrelativeRefCase c
renderRefCase (Appositive c)    = appositiveRefCase c
renderRefCase _                 = "a"  -- Default THM

transrelativeRefCase :: a -> Text
transrelativeRefCase _ = "a"  -- Simplified; full implementation needs case enum

appositiveRefCase :: a -> Text
appositiveRefCase _ = "ai"  -- Simplified

--------------------------------------------------------------------------------
-- Full Referential Structure
--------------------------------------------------------------------------------

data Referential
  = SingleRef PersonalRef Case Effect
  | DualReferential DualRef Case
  | CombRef CombinationRef Case
  deriving (Show, Eq)

data Effect
  = BEN1   -- ^ Beneficial to speaker
  | BEN2   -- ^ Beneficial to addressee
  | BEN3   -- ^ Beneficial to 3rd party
  | BENSL  -- ^ Self-beneficial
  | BENU   -- ^ Unknown/unspecified benefit
  | DET1   -- ^ Detrimental to speaker
  | DET2   -- ^ Detrimental to addressee
  | DET3   -- ^ Detrimental to 3rd party
  | DETSL  -- ^ Self-detrimental
  | DETU   -- ^ Unknown/unspecified detriment
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Effect vowel forms
effectVowel :: Effect -> Text
effectVowel BEN1  = "a"
effectVowel BEN2  = "e"
effectVowel BEN3  = "i"
effectVowel BENSL = "o"
effectVowel BENU  = "u"
effectVowel DET1  = "ai"
effectVowel DET2  = "ei"
effectVowel DET3  = "oi"
effectVowel DETSL = "ui"
effectVowel DETU  = "iu"

-- | Render a single referential
renderSingleRef :: PersonalRef -> Case -> Effect -> Text
renderSingleRef ref c eff =
  refC1 ref <> effectVowel eff <> renderRefCase c

--------------------------------------------------------------------------------
-- Demonstrative Referentials
--------------------------------------------------------------------------------

data Demonstrative
  = DemProximal    -- ^ This (near speaker)
  | DemMedial      -- ^ That (near addressee)
  | DemDistal      -- ^ That (far from both)
  | DemAbsent      -- ^ The aforementioned
  deriving (Show, Eq, Ord, Bounded, Enum)

demForm :: Demonstrative -> Text
demForm DemProximal = "th"
demForm DemMedial   = "kh"
demForm DemDistal   = "ph"
demForm DemAbsent   = "lh"
