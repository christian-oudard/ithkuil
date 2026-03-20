{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V3 (Elartkha) Rendering
-- Composes V3 formatives from structured data back to romanized text.
module Ithkuil.V3.Render
  ( renderFormative
  , renderVr
  , renderVc
  , renderCa
  , renderCiVi
  , renderVf
  , renderCb
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Ithkuil.V3.Grammar
import Ithkuil.V3.Parse (CaTables(..), vrTable, vcTable, civiTable, vfTable, cbTable)

-- | Render a complete V3 formative to romanized text.
-- Structure: [Tone] Vr + Cr + Vc (+CiVi) + Ca (+VxCx)* (+Vf (+Cb))
renderFormative :: CaTables -> Formative -> Maybe Text
renderFormative ca f = do
  vr <- renderVr (fVr f)
  let Root cr = fRoot f
  vc <- renderVc (fCase f)
  caC <- renderCa ca (fCa f)
  let civiPart = maybe (Just "") renderCiVi (fCiVi f)
  civi <- civiPart
  let affixParts = T.concat [affixVowel a <> affixConsonant a | a <- fAffixes f]
  let vfPart = maybe (Just "") renderVf (fVf f)
  vf <- vfPart
  let cbPart = maybe (Just "") renderCb (fBias f)
  cb <- cbPart
  return $ vr <> cr <> vc <> civi <> caC <> affixParts <> vf <> cb

-- | Render Vr slot (Function + Pattern + Stem) to vowel
renderVr :: SlotVr -> Maybe Text
renderVr key = case Map.lookup key vrTable of
  Just (v:_) -> Just v  -- Use first alternative
  _          -> Nothing

-- | Render Vc (Case) to vowel
renderVc :: Case -> Maybe Text
renderVc key = case Map.lookup key vcTable of
  Just (v:_) -> Just v
  _          -> Nothing

-- | Render Ca complex to consonant cluster
renderCa :: CaTables -> CaComplex -> Maybe Text
renderCa ca key = case Map.lookup key (caForward ca) of
  Just (v:_) -> Just v
  _          -> Nothing

-- | Render CiVi (Illocution + Mood) to consonant+vowel
renderCiVi :: SlotCiVi -> Maybe Text
renderCiVi key = case Map.lookup key civiTable of
  Just (v:_) -> Just v
  _          -> Nothing

-- | Render Vf (Context + Format) to vowel
renderVf :: SlotVf -> Maybe Text
renderVf key = case Map.lookup key vfTable of
  Just (v:_) -> Just v
  _          -> Nothing

-- | Render Cb (Bias) to consonant cluster
renderCb :: Bias -> Maybe Text
renderCb key = case Map.lookup key cbTable of
  Just (v:_) -> Just v
  _          -> Nothing
