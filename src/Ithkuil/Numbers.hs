{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Numbers
-- Centesimal (base-100) number system
module Ithkuil.Numbers where

import Data.Text (Text)
import qualified Data.Text as T

--------------------------------------------------------------------------------
-- Number Roots (0-99)
--------------------------------------------------------------------------------

-- | Basic number roots for 0-9 (from ch13 reference grammar)
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
  ]

-- | Teens (10-19): root J (=10) + digit root for 1-9
teenRoots :: [Text]
teenRoots =
  [ "j"     -- 10
  , "jll"   -- 11
  , "jks"   -- 12
  , "jz"    -- 13
  , "jpš"   -- 14
  , "jst"   -- 15
  , "jcp"   -- 16
  , "jns"   -- 17
  , "jčk"   -- 18
  , "jlẓ"   -- 19
  ]

-- | Tens (20, 30, ..., 90): v + digit root for 2-9
tensRoots :: [Text]
tensRoots =
  [ ""      -- 0 (not used directly)
  , ""      -- 10 (handled by teens)
  , "vks"   -- 20
  , "vz"    -- 30
  , "vpš"   -- 40
  , "vst"   -- 50
  , "vcp"   -- 60
  , "vns"   -- 70
  , "včk"   -- 80
  , "vlẓ"   -- 90
  ]

-- | Get root for numbers 0-99
numberRoot :: Int -> Text
numberRoot n
  | n < 0  = error "Negative number"
  | n < 10 = digitRoots !! n
  | n < 20 = teenRoots !! (n - 10)
  | n < 100 =
    let tens = n `div` 10
        ones = n `mod` 10
    in if ones == 0
       then tensRoots !! tens
       else tensRoots !! tens <> digitRoots !! ones
  | otherwise = error "Number >= 100 requires compound"

--------------------------------------------------------------------------------
-- Powers of 100
--------------------------------------------------------------------------------

-- | Roots for powers of 100 (from ch13 reference)
-- These are full roots, not affixes. The centesimal system uses:
-- 100^0: units (digit roots directly)
-- 100^1: -GZ- (hundred)
-- 100^2: -PC- (ten-thousand / myriad)
-- 100^4: -KẒ- (hundred-million)
-- 100^8: -ČG- (ten-quadrillion)
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
constructNumber :: Int -> NumberStem -> NumberVersion -> Text
constructNumber n stem ver =
  let vv = numberVv stem ver
      cr = numberRoot (n `mod` 100)
      vr = "a"  -- Basic specification
      ca = "l"  -- Default CA
  in vv <> cr <> vr <> ca

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

-- | Parse a number root to integer (0-99)
parseNumberRoot :: Text -> Maybe Int
parseNumberRoot t =
  case findIndex digitRoots t of
    Just n -> Just n
    Nothing -> case findIndex teenRoots t of
      Just n -> Just (n + 10)
      Nothing -> parseTensOnes t

findIndex :: [Text] -> Text -> Maybe Int
findIndex xs t = go 0 xs
  where
    go _ [] = Nothing
    go i (x:rest)
      | x == t = Just i
      | otherwise = go (i+1) rest

parseTensOnes :: Text -> Maybe Int
parseTensOnes t = go 2 tensRoots
  where
    go _ [] = Nothing
    go tens (r:rest)
      | tens >= 2, r /= "", r `isPrefixOf` t =
        let remainder = T.drop (T.length r) t
        in case findIndex digitRoots remainder of
          Just ones | ones > 0 -> Just (tens * 10 + ones)
          _ -> go (tens + 1) rest
      | otherwise = go (tens + 1) rest
    isPrefixOf p s = T.take (T.length p) s == p
