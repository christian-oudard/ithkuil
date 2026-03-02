{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil III (2011) Parser
-- Parses V3 formatives like "Elartkha" (the V3 name for Ithkuil)
module Ithkuil.V3.Parse
  ( parseV3Formative
  , parseV3SlotII
  , parseV3SlotIV
  , glossV3Word
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (listToMaybe)
import Ithkuil.V3.Grammar
import Ithkuil.Parse (splitConjuncts, isVowelChar)

--------------------------------------------------------------------------------
-- V3 Slot II Parsing (Pattern + Stem + Designation)
--------------------------------------------------------------------------------

-- | V3 Slot II vowel patterns
-- Format: vowel -> (Pattern, Stem, Designation)
v3SlotIIPatterns :: [(V3SlotII, Text)]
v3SlotIIPatterns =
  -- Pattern 1, Formal
  [ ((P1, S1, Formal), "a"),  ((P1, S1, Informal), "â")
  , ((P1, S2, Formal), "e"),  ((P1, S2, Informal), "ê")
  , ((P1, S3, Formal), "o"),  ((P1, S3, Informal), "ô")
  , ((P1, S0, Formal), "u"),  ((P1, S0, Informal), "û")
  -- Pattern 2, Formal
  , ((P2, S1, Formal), "ai"), ((P2, S1, Informal), "aì")
  , ((P2, S2, Formal), "ei"), ((P2, S2, Informal), "eì")
  , ((P2, S3, Formal), "oi"), ((P2, S3, Informal), "oì")
  , ((P2, S0, Formal), "ui"), ((P2, S0, Informal), "uì")
  -- Pattern 3, Formal
  , ((P3, S1, Formal), "au"), ((P3, S1, Informal), "aù")
  , ((P3, S2, Formal), "eu"), ((P3, S2, Informal), "eù")
  , ((P3, S3, Formal), "ou"), ((P3, S3, Informal), "où")
  , ((P3, S0, Formal), "iu"), ((P3, S0, Informal), "iù")
  ]

-- | Parse V3 Slot II vowel
parseV3SlotII :: Text -> Maybe V3SlotII
parseV3SlotII v = listToMaybe
  [ slot | (slot, vv) <- v3SlotIIPatterns, vv == v ]

--------------------------------------------------------------------------------
-- V3 Slot IV Parsing (Function + Pattern + Context)
--------------------------------------------------------------------------------

-- | V3 Slot IV vowel patterns (simplified - full version has more)
v3SlotIVPatterns :: [(V3SlotIV, Text)]
v3SlotIVPatterns =
  -- Stative, Pattern 1
  [ ((STA, P1, EXS), "a"),  ((STA, P1, FNC), "ai")
  , ((STA, P1, RPS), "ia"), ((STA, P1, AMG), "ua")
  -- Dynamic, Pattern 1
  , ((DYN, P1, EXS), "u"),  ((DYN, P1, FNC), "ui")
  , ((DYN, P1, RPS), "iu"), ((DYN, P1, AMG), "au")
  -- Stative, Pattern 2
  , ((STA, P2, EXS), "e"),  ((STA, P2, FNC), "ei")
  , ((STA, P2, RPS), "ie"), ((STA, P2, AMG), "ue")
  -- Dynamic, Pattern 2
  , ((DYN, P2, EXS), "o"),  ((DYN, P2, FNC), "oi")
  , ((DYN, P2, RPS), "io"), ((DYN, P2, AMG), "uo")
  ]

-- | Parse V3 Slot IV vowel
parseV3SlotIV :: Text -> Maybe V3SlotIV
parseV3SlotIV v = listToMaybe
  [ slot | (slot, vr) <- v3SlotIVPatterns, vr == v ]

--------------------------------------------------------------------------------
-- V3 Formative Parsing
--------------------------------------------------------------------------------

-- | Parse a V3 formative
-- V3 structure: (Cv) + Vv + Cr + Vr + Ca + (Vx+Cs) + Vc
-- For "Elartkha": e-l-a-rtkh-a
parseV3Formative :: Text -> Maybe V3Formative
parseV3Formative word = do
  let parts = splitConjuncts word
  case parts of
    [] -> Nothing
    (first:_) ->
      if isConsonantCluster first
        then parseV3ConsonantInitial parts
        else parseV3VowelInitial parts

-- | Check if text is a consonant cluster
isConsonantCluster :: Text -> Bool
isConsonantCluster t = case T.uncons t of
  Nothing -> False
  Just (c, _) -> not (isVowelChar c)

-- | Parse consonant-initial V3 word
parseV3ConsonantInitial :: [Text] -> Maybe V3Formative
parseV3ConsonantInitial parts = case parts of
  (cr:vr:rest) -> do
    slotIV <- parseV3SlotIV vr
    let (caRest, vcRest) = splitV3CaVc rest
        caseM = Nothing  -- TODO: parse case
    Just V3Formative
      { v3fSlotII = (P1, S1, Formal)  -- Default
      , v3fRoot = Root cr
      , v3fSlotIV = slotIV
      , v3fCa = Nothing  -- TODO: parse Ca
      , v3fCase = caseM
      , v3fRaw = parts
      }
  _ -> Nothing

-- | Parse vowel-initial V3 word
parseV3VowelInitial :: [Text] -> Maybe V3Formative
parseV3VowelInitial parts = case parts of
  (vv:cr:vr:rest) -> do
    slotII <- parseV3SlotII vv
    slotIV <- parseV3SlotIV vr
    let (caRest, vcRest) = splitV3CaVc rest
        caseM = Nothing  -- TODO: parse case
    Just V3Formative
      { v3fSlotII = slotII
      , v3fRoot = Root cr
      , v3fSlotIV = slotIV
      , v3fCa = Nothing
      , v3fCase = caseM
      , v3fRaw = parts
      }
  _ -> Nothing

-- | Split V3 rest into Ca and Vc
splitV3CaVc :: [Text] -> ([Text], [Text])
splitV3CaVc parts =
  let isVowelCluster t = not (T.null t) && isVowelChar (T.head t)
      revParts = reverse parts
      (_, afterC) = span (not . isVowelCluster) revParts
      (vcRev, caRev) = span isVowelCluster afterC
  in (reverse caRev, reverse vcRev)

--------------------------------------------------------------------------------
-- V3 Glossing
--------------------------------------------------------------------------------

-- | Generate a gloss for a V3 word
glossV3Word :: V3Formative -> Text
glossV3Word v3f =
  let (pat, stem, des) = v3fSlotII v3f
      Root cr = v3fRoot v3f
      (func, pat2, ctx) = v3fSlotIV v3f
  in T.concat
    [ T.pack (show pat), "/", T.pack (show stem), "/", T.pack (show des)
    , "-", cr
    , "-", T.pack (show func), "/", T.pack (show pat2), "/", T.pack (show ctx)
    ]
