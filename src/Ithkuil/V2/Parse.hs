{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil II (Ilaksh, 2007) Parser
-- Parses V2 formatives like "ilákš" (the V2 name for Ithkuil)
module Ithkuil.V2.Parse
  ( parseV2Formative
  , detectTone
  , glossV2Word
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Ithkuil.V2.Grammar
import Ithkuil.Parse (splitConjuncts, isVowelChar)

--------------------------------------------------------------------------------
-- Tone Detection
--------------------------------------------------------------------------------

-- | Detect tone from a vowel character
detectTone :: Char -> Tone
detectTone c
  | c `elem` ("áéíóú" :: String) = High
  | c `elem` ("àèìòù" :: String) = Low
  | c `elem` ("ǎěǐǒǔ" :: String) = Rising
  | c `elem` ("âêîôû" :: String) = Falling
  | otherwise = Neutral

-- | Strip tone from vowel to get base
stripTone :: Char -> Char
stripTone c = case c of
  'á' -> 'a'; 'à' -> 'a'; 'ǎ' -> 'a'; 'â' -> 'a'
  'é' -> 'e'; 'è' -> 'e'; 'ě' -> 'e'; 'ê' -> 'e'
  'í' -> 'i'; 'ì' -> 'i'; 'ǐ' -> 'i'; 'î' -> 'i'
  'ó' -> 'o'; 'ò' -> 'o'; 'ǒ' -> 'o'; 'ô' -> 'o'
  'ú' -> 'u'; 'ù' -> 'u'; 'ǔ' -> 'u'; 'û' -> 'u'
  _ -> c

--------------------------------------------------------------------------------
-- V2 Formative Parsing
--------------------------------------------------------------------------------

-- | Parse a V2 formative
-- "ilákš" breaks down as: i-l-á-kš
-- - i: initial vowel (Slot II marker)
-- - l: root consonant -L-
-- - á: stressed vowel with high tone
-- - kš: consonant cluster (Ca/case marker)
parseV2Formative :: Text -> Maybe V2Formative
parseV2Formative word = do
  let parts = splitConjuncts word
  case parts of
    [] -> Nothing
    _ -> parseV2Parts parts

-- | Parse V2 parts
parseV2Parts :: [Text] -> Maybe V2Formative
parseV2Parts parts = case parts of
  -- V2 pattern: V-C-V-C (vowel-consonant-vowel-consonant)
  (v1:c1:v2:_) | isVowelCluster v1 && not (isVowelCluster c1) -> do
    let root = c1
        -- Parse stem from first vowel
        stem = case T.head v1 of
          'a' -> S1
          'e' -> S2
          'i' -> S1  -- Special: i often = S1 in V2
          'o' -> S3
          'u' -> S0
          _ -> S1
        -- Detect tone in second vowel for version
        toneChar = T.head v2
        version = if detectTone toneChar == High then CPT else PRC
    Just V2Formative
      { v2fRoot = Root root
      , v2fStem = stem
      , v2fVersion = version
      , v2fFunc = STA
      , v2fRaw = parts
      }
  -- Consonant-initial
  (c1:v1:_) | not (isVowelCluster c1) -> do
    let root = c1
    Just V2Formative
      { v2fRoot = Root root
      , v2fStem = S1
      , v2fVersion = PRC
      , v2fFunc = STA
      , v2fRaw = parts
      }
  _ -> Nothing

-- | Check if text is a vowel cluster
isVowelCluster :: Text -> Bool
isVowelCluster t = case T.uncons t of
  Nothing -> False
  Just (c, _) -> isVowelChar c || isTonaVowel c

-- | Check for tonal vowels
isTonaVowel :: Char -> Bool
isTonaVowel c = c `elem` ("áéíóúàèìòùǎěǐǒǔâêîôû" :: String)

--------------------------------------------------------------------------------
-- V2 Glossing
--------------------------------------------------------------------------------

-- | Generate a gloss for a V2 word
glossV2Word :: V2Formative -> Text
glossV2Word v2f =
  let Root cr = v2fRoot v2f
  in T.concat
    [ T.pack (show (v2fStem v2f)), "/"
    , T.pack (show (v2fVersion v2f)), "-"
    , cr
    ]
