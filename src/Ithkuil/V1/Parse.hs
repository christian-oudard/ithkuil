{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil I (2004) Parser
-- Parses V1 formatives like "Iţkuîl" (the V1 name for Ithkuil)
module Ithkuil.V1.Parse
  ( parseV1Formative
  , detectGrade
  , applyGrade
  , glossV1Word
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Ithkuil.V1.Grammar
import Ithkuil.Parse (splitConjuncts, isVowelChar)

--------------------------------------------------------------------------------
-- Consonant Grade Detection
--------------------------------------------------------------------------------

-- | Detect consonant grade from a consonant cluster
-- Grade 8 is marked by dental prefix (tk, ţk) -> base is the second consonant
detectGrade :: Text -> (ConsonantGrade, Text)
detectGrade c
  -- Grade 8: dental addition (ţk -> base k, ţp -> base p, etc.)
  | "ţk" `T.isPrefixOf` c = (G8, "k")
  | "tk" `T.isPrefixOf` c = (G8, "k")
  | "ţp" `T.isPrefixOf` c = (G8, "p")
  | "ţt" `T.isPrefixOf` c = (G8, "t")
  | "ţ" `T.isPrefixOf` c = (G8, T.drop 1 c)  -- Generic: take after ţ
  -- Grade 7: fricative (x for k, etc.)
  | c == "x" = (G7, "k")
  | c == "f" = (G7, "p")
  | c == "s" = (G7, "t")
  -- Grade 2: geminate
  | T.length c >= 2 && T.head c == T.index c 1 = (G2, T.take 1 c)
  -- Default: Grade 1
  | otherwise = (G1, c)

-- | Apply a grade to a base consonant (for rendering)
applyGrade :: ConsonantGrade -> Text -> Text
applyGrade G1 c = c
applyGrade G2 c = c <> c
applyGrade G3 c = c <> "h"
applyGrade G4 c = c <> "'"
applyGrade G5 c = c <> "y"
applyGrade G6 c = c <> "w"
applyGrade G7 c = case c of
  "k" -> "x"
  "p" -> "f"
  "t" -> "s"
  _ -> c <> "~"
applyGrade G8 c = "ţ" <> c

--------------------------------------------------------------------------------
-- V1 Mode Detection
--------------------------------------------------------------------------------

-- | Detect mode from vocalic infix
-- u -> ui mutation indicates Secondary mode
detectMode :: Text -> V1Mode
detectMode v
  | "ui" `T.isInfixOf` v = Secondary
  | "uî" `T.isInfixOf` v = Secondary  -- With accent
  | otherwise = Primary

--------------------------------------------------------------------------------
-- V1 Formative Parsing
--------------------------------------------------------------------------------

-- | Parse a V1 formative
-- "Iţkuîl" etymology:
-- - i-: Delimitive extension + Coalescent affiliation prefix
-- - ţk: Grade 8 of k (dental addition)
-- - uî: Vocalic infix (ui = Secondary mode, stem 2)
-- - l: Second radical of root -K-L-
parseV1Formative :: Text -> Maybe V1Formative
parseV1Formative word = do
  let parts = splitConjuncts (T.toLower word)
  case parts of
    [] -> Nothing
    _ -> parseV1Parts parts

-- | Parse V1 parts
parseV1Parts :: [Text] -> Maybe V1Formative
parseV1Parts parts = case parts of
  -- Pattern with prefix: V-C-V-C (like "i-ţk-uî-l")
  (v1:c1:v2:c2:_) | isV1Vowel v1 && not (isV1Vowel c1) -> do
    let (grade, baseC1) = detectGrade c1
        mode = detectMode v2
        stem = detectStem v2
        root = V1Root baseC1 c2
    Just V1Formative
      { v1fRoot = root
      , v1fStem = stem
      , v1fGrade = grade
      , v1fMode = mode
      , v1fConfig = if grade == G8 then MSS else UNI  -- Grade 8 often = composite
      , v1fRaw = parts
      }
  -- Consonant-initial pattern: C-V-C
  (c1:v1:c2:_) | not (isV1Vowel c1) -> do
    let (grade, baseC1) = detectGrade c1
        mode = detectMode v1
        stem = detectStem v1
        root = V1Root baseC1 c2
    Just V1Formative
      { v1fRoot = root
      , v1fStem = stem
      , v1fGrade = grade
      , v1fMode = mode
      , v1fConfig = UNI
      , v1fRaw = parts
      }
  _ -> Nothing

-- | Check if text is a V1 vowel cluster
isV1Vowel :: Text -> Bool
isV1Vowel t = case T.uncons t of
  Nothing -> False
  Just (c, _) -> isVowelChar c || c `elem` ("îâêôû" :: String)

-- | Detect stem from vocalic infix
detectStem :: Text -> Stem
detectStem v
  | "u" `T.isPrefixOf` v = S2  -- u-infixes = Stem 2
  | "a" `T.isPrefixOf` v = S1
  | "i" `T.isPrefixOf` v = S3
  | "e" `T.isPrefixOf` v = S0
  | otherwise = S1

--------------------------------------------------------------------------------
-- V1 Glossing
--------------------------------------------------------------------------------

-- | Generate a gloss for a V1 word
glossV1Word :: V1Formative -> Text
glossV1Word v1f =
  let V1Root c1 c2 = v1fRoot v1f
  in T.concat
    [ T.pack (show (v1fStem v1f)), "/"
    , T.pack (show (v1fMode v1f)), "-"
    , c1, "-", c2
    , " [G", T.pack (show (fromEnum (v1fGrade v1f) + 1)), "]"
    ]
