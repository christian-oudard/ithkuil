{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Concatenation
-- Type 1 and Type 2 concatenated formatives
module Ithkuil.Concatenation where

import Data.Text (Text)
import qualified Data.Text as T
import Ithkuil.Grammar

--------------------------------------------------------------------------------
-- Concatenation Types
--------------------------------------------------------------------------------

-- | A concatenated word chain
data ConcatChain = ConcatChain
  { chainHead :: Formative           -- First formative (parent)
  , chainTail :: [(ConcatType, Formative)]  -- Linked formatives
  }
  deriving (Show, Eq)

-- | Type of concatenation
data ConcatType
  = Type1Concat  -- ^ Same case frame, forms compound concept
  | Type2Concat  -- ^ Independent case frame
  deriving (Show, Eq, Ord, Bounded, Enum)

--------------------------------------------------------------------------------
-- Concatenation Markers
--------------------------------------------------------------------------------

-- | Slot I consonant for concatenation status
concatMarker :: Maybe ConcatenationStatus -> Text
concatMarker Nothing      = ""       -- Not concatenated
concatMarker (Just Type1) = "h"      -- Type 1: shares case with parent
concatMarker (Just Type2) = "hw"     -- Type 2: independent case

-- | Check if formative is concatenated
isConcatenated :: Formative -> Bool
isConcatenated f = fSlotI f /= Nothing

-- | Get concatenation type
getConcatType :: Formative -> Maybe ConcatenationStatus
getConcatType = fSlotI

--------------------------------------------------------------------------------
-- Chain Construction
--------------------------------------------------------------------------------

-- | Create a new chain with head formative
newChain :: Formative -> ConcatChain
newChain headF = ConcatChain headF []

-- | Add Type 1 concatenated formative
addType1 :: ConcatChain -> Formative -> ConcatChain
addType1 chain f = chain
  { chainTail = chainTail chain ++ [(Type1Concat, f { fSlotI = Just Type1 })]
  }

-- | Add Type 2 concatenated formative
addType2 :: ConcatChain -> Formative -> ConcatChain
addType2 chain f = chain
  { chainTail = chainTail chain ++ [(Type2Concat, f { fSlotI = Just Type2 })]
  }

--------------------------------------------------------------------------------
-- Chain Analysis
--------------------------------------------------------------------------------

-- | Get all formatives in chain
chainFormatives :: ConcatChain -> [Formative]
chainFormatives chain = chainHead chain : map snd (chainTail chain)

-- | Chain length
chainLength :: ConcatChain -> Int
chainLength = length . chainFormatives

-- | Get Type 1 dependents (share case frame)
type1Dependents :: ConcatChain -> [Formative]
type1Dependents chain =
  [f | (Type1Concat, f) <- chainTail chain]

-- | Get Type 2 dependents (independent case frames)
type2Dependents :: ConcatChain -> [Formative]
type2Dependents chain =
  [f | (Type2Concat, f) <- chainTail chain]

--------------------------------------------------------------------------------
-- Rendering
--------------------------------------------------------------------------------

-- | Render concatenation chain to text
renderChain :: ConcatChain -> Text
renderChain chain =
  let headText = renderFormativeSimple (chainHead chain)
      tailTexts = [concatMarker (Just ct) <> renderFormativeSimple f
                  | (ct', f) <- chainTail chain
                  , let ct = case ct' of
                          Type1Concat -> Type1
                          Type2Concat -> Type2
                  ]
  in T.intercalate "-" (headText : tailTexts)

-- | Simple formative rendering (placeholder)
renderFormativeSimple :: Formative -> Text
renderFormativeSimple f =
  let (Root cr) = fSlotIII f
  in "a" <> cr <> "a" <> "l"  -- Minimal: a-Cr-a-l

--------------------------------------------------------------------------------
-- Parsing Chains
--------------------------------------------------------------------------------

-- | Parse a potential concatenation chain
parseChain :: Text -> Maybe ConcatChain
parseChain word =
  -- Check for concatenation markers
  if "hw" `T.isInfixOf` word || hasInternalH word
  then Just $ ConcatChain (minimalFormative "?") []  -- Placeholder
  else Nothing
  where
    hasInternalH w =
      let parts = T.splitOn "-" w
      in length parts > 1

--------------------------------------------------------------------------------
-- Type 1 vs Type 2 Semantics
--------------------------------------------------------------------------------

{- |
Type 1 Concatenation:
- Child shares case frame with parent
- Forms compound semantic concept
- Similar to English noun-noun compounds: "tree house" = tree-type house

Type 2 Concatenation:
- Child has independent case frame
- Case is taken from the final Vc of the child
- More like coordination or separate argument

Example Type 1: "the tall-woman's house" where tall-woman is one concept
Example Type 2: "the woman's tall house" where tall modifies house independently
-}

-- | Semantic type of chain
data ChainSemantics
  = CompoundConcept      -- ^ All Type 1: single compound concept
  | CoordinatedConcepts  -- ^ All Type 2: separate but related
  | MixedChain           -- ^ Mix of Type 1 and Type 2
  deriving (Show, Eq)

chainSemantics :: ConcatChain -> ChainSemantics
chainSemantics chain
  | null (chainTail chain) = CompoundConcept
  | all isType1 (chainTail chain) = CompoundConcept
  | all isType2 (chainTail chain) = CoordinatedConcepts
  | otherwise = MixedChain
  where
    isType1 (Type1Concat, _) = True
    isType1 _ = False
    isType2 (Type2Concat, _) = True
    isType2 _ = False
