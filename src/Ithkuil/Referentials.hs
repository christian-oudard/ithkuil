{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Referentials (Personal Reference)
-- Per Chapter 9 of the official grammar
module Ithkuil.Referentials
  ( Referent(..)
  , ReferentEffect(..)
  , PersonalRef(..)
  , ReferentCategory(..)
  , refC1
  , refC1All
  , lookupRefC1
  , decomposeRefCluster
  , decomposeRefWithCategory
  , categoryLabel
  , biconsonantalRefs
  , renderRefCase
  , referentLabel
  , referentAbbrev
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Set as Set
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
-- Includes alternate form: ļ = pi.NEU (alongside ẓ)
lookupRefC1 :: Text -> Maybe PersonalRef
lookupRefC1 "ļ" = Just (PersonalRef Rpi NEU)  -- alternate form
lookupRefC1 c = listToMaybe [pr | (pr, form) <- refC1All, form == c]

-- | Biconsonantal referential forms (must be checked before single-char)
biconsonantalRefs :: Set.Set Text
biconsonantalRefs = Set.fromList
  [ "tļ"
  , "th", "ph", "kh"
  , "ll", "rr", "řř"
  , "mm", "nn", "ňň"
  , "hl", "hm", "hn", "hň"
  ]

-- | Decompose a consonant cluster into individual referent consonants
-- Uses greedy left-to-right matching: biconsonantal forms checked first
decomposeRefCluster :: Text -> Maybe [PersonalRef]
decomposeRefCluster t
  | T.null t = Just []
  | T.length t >= 2
  , let bi = T.take 2 t
  , bi `Set.member` biconsonantalRefs
  = case lookupRefC1 bi of
      Just ref -> (ref :) <$> decomposeRefCluster (T.drop 2 t)
      Nothing -> Nothing
  | otherwise
  = case lookupRefC1 (T.take 1 t) of
      Just ref -> (ref :) <$> decomposeRefCluster (T.drop 1 t)
      Nothing -> Nothing

--------------------------------------------------------------------------------
-- Referential Case Marking
--------------------------------------------------------------------------------

-- | Render case for referential (uses same Vc vowels as formative Slot IX)
renderRefCase :: Case -> Text
renderRefCase = renderCase

-- | Human-readable label for a referent category
referentLabel :: Referent -> Text
referentLabel R1m  = "I"
referentLabel R2m  = "you(sg.)"
referentLabel R2p  = "you(pl.)"
referentLabel Rma  = "he/she"
referentLabel Rpa  = "they(anim.)"
referentLabel Rmi  = "it"
referentLabel Rpi  = "them(inanim.)"
referentLabel Rmx  = "it+they(mixed)"
referentLabel Rrdp = "aforementioned"
referentLabel Robv = "the other one"
referentLabel Rpvs = "whatever"

-- | Short abbreviation for a referent (matches standard Ithkuil notation)
referentAbbrev :: Referent -> Text
referentAbbrev R1m  = "1m"
referentAbbrev R2m  = "2m"
referentAbbrev R2p  = "2p"
referentAbbrev Rma  = "ma"
referentAbbrev Rpa  = "pa"
referentAbbrev Rmi  = "mi"
referentAbbrev Rpi  = "pi"
referentAbbrev Rmx  = "Mx"
referentAbbrev Rrdp = "Rdp"
referentAbbrev Robv = "Obv"
referentAbbrev Rpvs = "PVS"

--------------------------------------------------------------------------------
-- Referent Categories: Agglomerative, Nomic, Abstract (per Sec. 4.6)
--------------------------------------------------------------------------------

-- | Category modifiers for referentials
-- Added as prefix or suffix to C1 consonant cluster:
--   Agglomerative: -ļ- / -tļ-  ("each/every")
--   Nomic:         -ç- / -x-   ("one/someone/something")
--   Abstract:      -w  / -y    ("everything about X")
data ReferentCategory
  = Agglomerative  -- ^ each/every referent individually
  | Nomic          -- ^ indefinite ("someone", "something"); replaces old IPa/IPi
  | Abstract       -- ^ "everything about X", "all that X is"
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Gloss label for a referent category
categoryLabel :: ReferentCategory -> Text
categoryLabel Agglomerative = "AGM"
categoryLabel Nomic         = "NOM"
categoryLabel Abstract      = "ABS"

-- | Try to decompose a consonant cluster as referential C1 with optional category
-- First tries normal decomposition; if that fails, tries stripping
-- agglomerative/nomic/abstract prefixes and suffixes.
decomposeRefWithCategory :: Text -> Maybe (Maybe ReferentCategory, [PersonalRef])
decomposeRefWithCategory t =
  case decomposeRefCluster t of
    Just refs@(_:_) -> Just (Nothing, refs)
    _ -> tryPrefixes t
  where
    tryPrefixes c
      -- Prefixes (longest first)
      | Just rest <- T.stripPrefix "tļ" c, ok rest = Just (Just Agglomerative, fromJ rest)
      | Just rest <- T.stripPrefix "ç" c,  ok rest = Just (Just Nomic, fromJ rest)
      | Just rest <- T.stripPrefix "x" c,  ok rest = Just (Just Nomic, fromJ rest)
      | Just rest <- T.stripPrefix "w" c,  ok rest = Just (Just Abstract, fromJ rest)
      | Just rest <- T.stripPrefix "y" c,  ok rest = Just (Just Abstract, fromJ rest)
      | Just rest <- T.stripPrefix "ļ" c,  not (T.null rest), ok rest = Just (Just Agglomerative, fromJ rest)
      | otherwise = trySuffixes c
    trySuffixes c
      -- Suffixes (longest first)
      | Just rest <- T.stripSuffix "tļ" c, ok rest = Just (Just Agglomerative, fromJ rest)
      | Just rest <- T.stripSuffix "ç" c,  ok rest = Just (Just Nomic, fromJ rest)
      | Just rest <- T.stripSuffix "x" c,  ok rest = Just (Just Nomic, fromJ rest)
      | Just rest <- T.stripSuffix "w" c,  ok rest = Just (Just Abstract, fromJ rest)
      | Just rest <- T.stripSuffix "y" c,  ok rest = Just (Just Abstract, fromJ rest)
      | Just rest <- T.stripSuffix "ļ" c,  not (T.null rest), ok rest = Just (Just Agglomerative, fromJ rest)
      | otherwise = Nothing
    ok r = case decomposeRefCluster r of { Just (_:_) -> True; _ -> False }
    fromJ r = case decomposeRefCluster r of { Just rs -> rs; Nothing -> [] }
