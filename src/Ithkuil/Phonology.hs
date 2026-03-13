{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Phonology
-- 9 vowels, 31 consonants
module Ithkuil.Phonology where

import Data.Text (Text)

-- | Consonant features
data Voicing = Voiced | Voiceless
  deriving (Show, Eq, Ord, Enum, Bounded)

data Place
  = Labial | LabioDental | Dental | Alveolar | PostAlveolar
  | Retroflex | Palatal | Velar | Uvular | Glottal
  deriving (Show, Eq, Ord, Enum, Bounded)

data Manner
  = Stop | Fricative | Affricate | Nasal | Tap | Trill
  | Approximant | LateralApprox | LateralFric
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Vowel features
data Height = High | Mid | Low
  deriving (Show, Eq, Ord, Enum, Bounded)

data Backness = Front | Central | Back
  deriving (Show, Eq, Ord, Enum, Bounded)

data Rounding = Rounded | Unrounded
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Phoneme type
data Phoneme
  = Consonant Voicing Place Manner
  | Vowel Height Backness Rounding
  deriving (Show, Eq, Ord)

-- | The 31 consonants of Ithkuil V4
consonants :: [(Phoneme, Char, Text)]
consonants =
  [ (Consonant Voiceless Labial Stop,        'p', "p")
  , (Consonant Voiced Labial Stop,           'b', "b")
  , (Consonant Voiceless Alveolar Stop,      't', "t")
  , (Consonant Voiced Alveolar Stop,         'd', "d")
  , (Consonant Voiceless Velar Stop,         'k', "k")
  , (Consonant Voiced Velar Stop,            'g', "g")
  , (Consonant Voiceless Glottal Stop,       '\'', "'")
  , (Consonant Voiceless LabioDental Fricative, 'f', "f")
  , (Consonant Voiced LabioDental Fricative,    'v', "v")
  , (Consonant Voiceless Dental Fricative,      'T', "\x0163") -- ţ
  , (Consonant Voiced Dental Fricative,         'D', "\x1E11") -- ḑ
  , (Consonant Voiceless Alveolar Fricative,    's', "s")
  , (Consonant Voiced Alveolar Fricative,       'z', "z")
  , (Consonant Voiceless PostAlveolar Fricative,'S', "\x0161") -- š
  , (Consonant Voiced PostAlveolar Fricative,   'Z', "\x017E") -- ž
  , (Consonant Voiceless Palatal Fricative,     'c', "\x00E7") -- ç
  , (Consonant Voiceless Uvular Fricative,      'x', "x")
  , (Consonant Voiceless Glottal Fricative,     'h', "h")
  , (Consonant Voiceless Alveolar Affricate,    'C', "c")
  , (Consonant Voiced Alveolar Affricate,       'J', "\x1E93") -- ẓ
  , (Consonant Voiceless PostAlveolar Affricate,'X', "\x010D") -- č
  , (Consonant Voiced PostAlveolar Affricate,   'j', "j")
  , (Consonant Voiced Labial Nasal,             'm', "m")
  , (Consonant Voiced Alveolar Nasal,           'n', "n")
  , (Consonant Voiced Velar Nasal,              'N', "\x0148") -- ň
  , (Consonant Voiced Alveolar Tap,             'r', "r")
  , (Consonant Voiced Uvular Trill,             'R', "\x0159") -- ř
  , (Consonant Voiced Alveolar LateralApprox,   'l', "l")
  , (Consonant Voiced Palatal Approximant,      'y', "y")
  , (Consonant Voiced Labial Approximant,       'w', "w")
  , (Consonant Voiceless Alveolar LateralFric,  'L', "\x013C") -- ļ
  ]

-- | The 9 vowels of Ithkuil V4
vowels :: [(Phoneme, Char, Text)]
vowels =
  [ (Vowel High Front Unrounded,    'i', "i")
  , (Vowel High Central Unrounded,  'I', "\x00FC") -- ü
  , (Vowel High Back Rounded,       'u', "u")
  , (Vowel Mid Front Unrounded,     'e', "e")
  , (Vowel Mid Central Unrounded,   'E', "\x00EB") -- ë
  , (Vowel Mid Back Rounded,        'o', "o")
  , (Vowel Mid Front Rounded,       'O', "\x00F6") -- ö
  , (Vowel Low Central Unrounded,   'a', "a")
  , (Vowel Low Back Unrounded,      'A', "\x00E4") -- ä
  ]

-- | Vowel form table (Series 1-4, Forms 1-9)
-- Used for encoding grammatical categories
vowelFormTable :: [[Text]]
vowelFormTable =
  [ ["a",  "ä",  "e",  "ë",  "i",  "ö",  "o",  "ü",  "u" ]  -- Series 1
  , ["ai", "au", "ei", "eu", "ëi", "ou", "oi", "iu", "ui"]  -- Series 2
  , ["ia", "iä", "ie", "ië", "ëu", "uö", "uo", "ue", "ua"]  -- Series 3
  , ["ao", "ae", "ea", "eo", "eë", "öe", "oe", "öa", "oa"]  -- Series 4
  ]

-- | Get vowel form by series (1-4) and form (1-9)
vowelForm :: Int -> Int -> Text
vowelForm series form = vowelFormTable !! (series - 1) !! (form - 1)

-- | Check if a phoneme is a consonant
isConsonant :: Phoneme -> Bool
isConsonant (Consonant _ _ _) = True
isConsonant _ = False

-- | Check if a phoneme is a vowel
isVowel :: Phoneme -> Bool
isVowel (Vowel _ _ _) = True
isVowel _ = False
