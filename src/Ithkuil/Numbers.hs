{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Numbers
-- Centesimal (base-100) number system
module Ithkuil.Numbers
  ( numberRoot
  , numberAffix
  , powerRoots
  , digitRoots
  , parseNumberRoot
  , NumberStem(..)
  , NumberVersion(..)
  , constructNumber
  , numberVv
  , monthAffixes
  , dayOfWeekAffixes
  ) where

import Data.Text (Text)

--------------------------------------------------------------------------------
-- Number Roots (0-99)
--------------------------------------------------------------------------------

-- | Basic number roots for 0-10 (from ch13 reference grammar)
digitRoots :: [Text]
digitRoots =
  [ "vr"   -- 0
  , "ll"   -- 1
  , "ks"   -- 2
  , "z"    -- 3
  , "pš"   -- 4
  , "st"   -- 5
  , "cp"   -- 6
  , "ns"   -- 7
  , "čk"   -- 8
  , "lẓ"   -- 9
  , "j"    -- 10
  ]

-- | Get root consonant for a number 0-99.
-- 0-10: direct root. 11-99: digit root (ones digit, or 0 for multiples of 10).
-- Returns Nothing for negative numbers or >= 100.
numberRoot :: Int -> Maybe Text
numberRoot n
  | n < 0    = Nothing
  | n <= 10  = Just (digitRoots !! n)
  | n < 100  =
    let ones = n `mod` 10
    in Just (digitRoots !! ones)
  | otherwise = Nothing

-- | Get TNX affix (-rs-) for numbers 11-99.
-- Returns Nothing for 0-10, Just (cs, degree) for 11-99.
-- The TNX affix Cs is "rs" and degree N adds N*10.
numberAffix :: Int -> Maybe (Text, Int)
numberAffix n
  | n <= 10  = Nothing
  | n < 100  =
    let tens = n `div` 10
    in Just ("rs", tens)
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Powers of 100
--------------------------------------------------------------------------------

-- | Roots for powers of 100 (from ch13 reference)
powerRoots :: [Text]
powerRoots =
  [ ""      -- 100^0 = units
  , "gz"    -- 100^1 = 100
  , "pc"    -- 100^2 = 10,000
  , "kẓ"    -- 100^4 = 100,000,000
  , "čg"    -- 100^8 = 10,000,000,000,000,000
  ]

--------------------------------------------------------------------------------
-- Number Formative Construction
--------------------------------------------------------------------------------

-- | Stem for number type
data NumberStem
  = NSCardinal    -- ^ Cardinal: one, two, three
  | NSOrdinal     -- ^ Ordinal: first, second, third
  | NSPartitive   -- ^ Partitive: one of, two of
  | NSCollective  -- ^ Collective: group of N
  deriving (Show, Eq, Ord, Bounded, Enum)

-- | Version for number
data NumberVersion
  = NVConcrete    -- ^ Specific count
  | NVAbstract    -- ^ Abstract/approximate
  deriving (Show, Eq, Ord)

-- | Construct a number formative
constructNumber :: Int -> NumberStem -> NumberVersion -> Maybe Text
constructNumber n stem ver = do
  cr <- numberRoot (n `mod` 100)
  let vv = numberVv stem ver
      vr = "a"  -- Basic specification
      ca = "l"  -- Default CA
  pure (vv <> cr <> vr <> ca)

numberVv :: NumberStem -> NumberVersion -> Text
numberVv NSCardinal NVConcrete   = "a"
numberVv NSCardinal NVAbstract   = "u"
numberVv NSOrdinal NVConcrete    = "e"
numberVv NSOrdinal NVAbstract    = "i"
numberVv NSPartitive NVConcrete  = "o"
numberVv NSPartitive NVAbstract  = "ö"
numberVv NSCollective NVConcrete = "ä"
numberVv NSCollective NVAbstract = "ü"

--------------------------------------------------------------------------------
-- Date/Time Numbers (special forms)
--------------------------------------------------------------------------------

-- | Month affixes (1-12)
monthAffixes :: [Text]
monthAffixes =
  [ "lks"   -- Month 1 (January equivalent)
  , "lz"    -- Month 2
  , "lps"   -- Month 3
  , "lst"   -- Month 4
  , "lcp"   -- Month 5
  , "lns"   -- Month 6
  , "lčk"   -- Month 7
  , "llẓ"   -- Month 8
  , "lpc"   -- Month 9
  , "lj"    -- Month 10
  , "ljks"  -- Month 11
  , "ljz"   -- Month 12
  ]

-- | Day of week affixes (1-7)
dayOfWeekAffixes :: [Text]
dayOfWeekAffixes =
  [ "mks"   -- Day 1 (Monday equivalent)
  , "mz"    -- Day 2
  , "mps"   -- Day 3
  , "mst"   -- Day 4
  , "mcp"   -- Day 5
  , "mns"   -- Day 6
  , "mčk"   -- Day 7
  ]

--------------------------------------------------------------------------------
-- Parsing Numbers
--------------------------------------------------------------------------------

-- | Parse a number root to integer (0-10 only, for now)
parseNumberRoot :: Text -> Maybe Int
parseNumberRoot t = findIndex digitRoots t

findIndex :: [Text] -> Text -> Maybe Int
findIndex xs t = go 0 xs
  where
    go _ [] = Nothing
    go i (x:rest)
      | x == t = Just i
      | otherwise = go (i+1) rest
