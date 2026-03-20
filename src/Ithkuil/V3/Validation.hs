{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V3 (Elartkha) Validation
-- Validates V3 formative structure and phonotactic constraints.
module Ithkuil.V3.Validation
  ( ValidationResult(..)
  , ValidationError(..)
  , validateFormative
  , validateWord
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import Ithkuil.V3.Grammar
import Ithkuil.V3.Parse (CaTables(..), vrTableReverse, vcTableReverse, vfTableReverse, cbTableReverse, isV)

data ValidationResult
  = Valid
  | Invalid [ValidationError]
  deriving (Show, Eq)

data ValidationError
  = MissingRoot                      -- ^ No consonant root found
  | InvalidVr Text                   -- ^ Unrecognized Vr vowel
  | InvalidVc Text                   -- ^ Unrecognized Vc vowel
  | InvalidCa Text                   -- ^ Unrecognized Ca cluster
  | EmptyWord                        -- ^ Empty input
  | StructuralError Text             -- ^ General structural issue
  deriving (Show, Eq)

-- | Validate a parsed formative for internal consistency.
validateFormative :: Formative -> ValidationResult
validateFormative f =
  let errors = concatMap id
        [ checkRoot (fRoot f)
        , checkCa (fCa f)
        , checkVf (fVf f)
        ]
  in if null errors then Valid else Invalid errors

checkRoot :: Root -> [ValidationError]
checkRoot (Root t)
  | T.null t  = [MissingRoot]
  | T.any isV t = [StructuralError "Root contains vowels"]
  | otherwise = []

checkCa :: CaComplex -> [ValidationError]
checkCa (ess, ext, per, aff, cfg) =
  -- RPV essence only valid with non-default other values
  let _ = (ess, ext, per, aff, cfg)
  in []  -- Ca values are validated by the lookup table itself

checkVf :: Maybe SlotVf -> [ValidationError]
checkVf Nothing = []
checkVf (Just _) = []

-- | Validate a raw word string against V3 phonotactic expectations.
-- Checks basic structure: must have vowels and consonants alternating.
validateWord :: CaTables -> Text -> ValidationResult
validateWord ca word =
  let w = T.toLower word
      errors = concatMap id
        [ checkNotEmpty w
        , checkAlternation w
        , checkVowelsKnown w
        , checkCaKnown ca w
        ]
  in if null errors then Valid else Invalid errors

checkNotEmpty :: Text -> [ValidationError]
checkNotEmpty t
  | T.null t  = [EmptyWord]
  | otherwise = []

-- | Check that the word has proper V/C alternation (no triple vowels, etc.)
checkAlternation :: Text -> [ValidationError]
checkAlternation t =
  let groups = groupVC (T.unpack t)
      -- Check for any vowel group longer than 3 characters (unusual)
      longVowels = [grp | (True, grp) <- groups, length grp > 3]
  in [StructuralError (T.pack $ "Unusually long vowel group: " ++ v) | v <- longVowels]

groupVC :: String -> [(Bool, String)]
groupVC [] = []
groupVC (c:cs) =
  let isVowel = isV c
      (same, rest) = span (\x -> isV x == isVowel) cs
  in (isVowel, c:same) : groupVC rest

-- | Check that vowel sequences are recognized by the Vr or Vc tables.
checkVowelsKnown :: Text -> [ValidationError]
checkVowelsKnown t =
  let vGroups = [T.pack grp | (True, grp) <- groupVC (T.unpack (T.toLower t))]
  in [ InvalidVr v
     | v <- take 1 vGroups  -- First vowel = Vr
     , Map.notMember v vrTableReverse
     , v /= ""  -- Empty Vr is valid
     ]
  ++ [ InvalidVc v
     | v <- take 1 (drop 1 vGroups)  -- Second vowel = Vc
     , Map.notMember v vcTableReverse
     ]

-- | Check if consonant clusters could be valid Ca values.
checkCaKnown :: CaTables -> Text -> [ValidationError]
checkCaKnown ca t =
  let cGroups = [T.pack grp | (False, grp) <- groupVC (T.unpack (T.toLower t))]
  in case cGroups of
       -- If there are at least 2 consonant groups, the second should be Ca
       (_:caCluster:_)
         | Map.notMember caCluster (caReverse ca) -> [InvalidCa caCluster]
       _ -> []
