{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V3 (Elartkha) Phonology
--
-- 45 consonants, 13 basic vowels + diphthongs + tone-marked variants.
-- Tone system: 6 tones encoding Version.
-- ASCII romanization with diacritics via ^, :, /, \, ~, = markers.
module Ithkuil.V3.Phonology
  ( -- * Phoneme inventories
    consonants
  , vowels
    -- * ASCII/Unicode conversion
  , asciiToUnicode
  , unicodeToAscii
    -- * Tone
  , ToneMark(..)
  , toneMarkUnicode
  , toneMarkAscii
    -- * Classification
  , isVowel
  , isConsonant
    -- * Conjunct splitting
  , splitConjuncts
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Char (isAlpha)

--------------------------------------------------------------------------------
-- Consonant inventory (45 consonants)
--------------------------------------------------------------------------------

-- | (ASCII, Unicode) pairs for all V3 consonants
consonantPairs :: [(Text, Text)]
consonantPairs =
  [ ("p",    "p"),   ("b",    "b"),   ("ph",   "pʰ"),  ("p'",   "p'")
  , ("m",    "m"),   ("w",    "w"),   ("f",    "f"),    ("v",    "v")
  , ("t",    "t"),   ("d",    "d"),   ("th",   "tʰ"),   ("t'",   "t'")
  , ("t,",   "ţ"),   ("dh",   "dh"),  ("n",    "n")
  , ("c",    "c"),   ("dz",   "ż"),   ("ch",   "cʰ"),   ("c'",   "c'")
  , ("s",    "s"),   ("z",    "z"),   ("r",    "r")
  , ("c^",   "č"),   ("j",    "j"),   ("c^h",  "čʰ"),   ("c^'",  "č'")
  , ("s^",   "š"),   ("z^",   "ž"),   ("c,",   "ç"),    ("y",    "y")
  , ("k",    "k"),   ("g",    "g"),   ("kh",   "kʰ"),   ("k'",   "k'")
  , ("x",    "x"),   ("n^",   "ň")
  , ("q",    "q"),   ("qh",   "qʰ"),  ("q'",   "q'")
  , ("xh",   "xh"),  ("r^",   "ř")
  , ("'",    "'"),   ("h",    "h"),   ("l,",   "ļ"),    ("l",    "l")
  ]

consonants :: [Text]
consonants = map snd consonantPairs

--------------------------------------------------------------------------------
-- Vowel inventory (13 basic + diphthongs + tone/stress variants)
--------------------------------------------------------------------------------

-- | (ASCII, Unicode) pairs for V3 vowels
vowelPairs :: [(Text, Text)]
vowelPairs =
  [ ("a",   "a"),   ("a^",  "â"),   ("e",   "e"),   ("e^",  "ê")
  , ("e:",  "ë"),   ("i",   "i"),   ("i^",  "î"),   ("o",   "o")
  , ("o^",  "ô"),   ("o:",  "ö"),   ("u",   "u"),   ("u^",  "û")
  , ("u:",  "ü")
  -- Stressed variants (acute accent = high tone mark on vowel)
  , ("a/",  "á"),   ("a^/", "ââ"),  ("e/",  "é"),   ("e^/", "êê")
  , ("e:/", "ëë"),  ("i/",  "í"),   ("i^/", "îî"),  ("o/",  "ó")
  , ("o^/", "ôô"),  ("o:/", "öö"),  ("u/",  "ú"),   ("u^/", "ûû")
  , ("u:/", "üü")
  -- Grave accent variants
  , ("a\\", "à"),   ("e\\", "è"),   ("i\\", "ì"),   ("o\\", "ò")
  , ("u\\", "ù")
  -- Special
  , ("ae",  "æ"),   ("o%",  "ø"),   ("i+",  "ɨ")
  ]

vowels :: [Text]
vowels = map snd vowelPairs

--------------------------------------------------------------------------------
-- ASCII/Unicode conversion maps
--------------------------------------------------------------------------------

asciiToUnicodeMap :: Map Text Text
asciiToUnicodeMap = Map.fromList (consonantPairs ++ vowelPairs)

unicodeToAsciiMap :: Map Text Text
unicodeToAsciiMap = Map.fromList [(u, a) | (a, u) <- consonantPairs ++ vowelPairs]

-- | Convert ASCII romanization to Unicode
asciiToUnicode :: Text -> Text
asciiToUnicode = convertWith asciiToUnicodeMap

-- | Convert Unicode to ASCII romanization
unicodeToAscii :: Text -> Text
unicodeToAscii = convertWith unicodeToAsciiMap

-- | Generic multi-character substitution (longest match first)
convertWith :: Map Text Text -> Text -> Text
convertWith table input = go input
  where
    -- Try longest matches first (up to 3 chars)
    go t | T.null t = ""
    go t =
      case tryLen 3 t `orElse` tryLen 2 t `orElse` tryLen 1 t of
        Just (repl, rest) -> repl <> go rest
        Nothing           -> T.take 1 t <> go (T.drop 1 t)
    tryLen n t
      | T.length t >= n =
          let chunk = T.take n t
          in case Map.lookup chunk table of
               Just repl -> Just (repl, T.drop n t)
               Nothing   -> Nothing
      | otherwise = Nothing
    orElse (Just x) _ = Just x
    orElse Nothing  y = y

--------------------------------------------------------------------------------
-- Tone marks
--------------------------------------------------------------------------------

data ToneMark
  = ToneFalling        -- ^ Default (unmarked)
  | ToneHigh           -- ^ Macron (=)
  | ToneRising         -- ^ Acute (/)
  | ToneLow            -- ^ Underscore (_)
  | ToneFallingRising  -- ^ Caron (~)
  | ToneRisingFalling  -- ^ Circumflex (^)
  deriving (Show, Eq, Ord, Enum, Bounded)

toneMarkUnicode :: ToneMark -> Text
toneMarkUnicode ToneFalling       = ""       -- unmarked
toneMarkUnicode ToneHigh          = "\x0304" -- combining macron
toneMarkUnicode ToneRising        = "\x0301" -- combining acute
toneMarkUnicode ToneLow           = "\x0332" -- combining low line
toneMarkUnicode ToneFallingRising = "\x030C" -- combining caron
toneMarkUnicode ToneRisingFalling = "\x0302" -- combining circumflex

toneMarkAscii :: ToneMark -> Text
toneMarkAscii ToneFalling       = ""
toneMarkAscii ToneHigh          = "="
toneMarkAscii ToneRising        = "/"
toneMarkAscii ToneLow           = "_"
toneMarkAscii ToneFallingRising = "~"
toneMarkAscii ToneRisingFalling = "^"

--------------------------------------------------------------------------------
-- Phoneme classification
--------------------------------------------------------------------------------

vowelChars :: [Char]
vowelChars = "aâäeêëiîoôöuûüáéíóúàèìòùæøɨ"

isVowel :: Char -> Bool
isVowel c = c `elem` vowelChars

isConsonant :: Char -> Bool
isConsonant c = isAlpha c && not (isVowel c)

--------------------------------------------------------------------------------
-- Conjunct splitting (V/C alternation)
--------------------------------------------------------------------------------

-- | Split a word into alternating vowel and consonant sequences.
-- Returns a list of (isVowel, text) pairs.
splitConjuncts :: Text -> [(Bool, Text)]
splitConjuncts = go . T.unpack
  where
    go [] = []
    go (c:cs)
      | isVowel c =
          let (vs, rest) = span isVowel cs
          in (True, T.pack (c:vs)) : go rest
      | otherwise =
          let (cs', rest) = span (\x -> not (isVowel x) && isAlpha x) cs
          in (False, T.pack (c:cs')) : go rest
