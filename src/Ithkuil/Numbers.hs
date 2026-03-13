{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Numbers
-- Centesimal (base-100) number system
module Ithkuil.Numbers where

import Data.Text (Text)

--------------------------------------------------------------------------------
-- Number Roots (0-99)
--------------------------------------------------------------------------------

-- | Basic number roots for 0-9
digitRoots :: [Text]
digitRoots =
  [ "ll"   -- 0
  , "ks"   -- 1
  , "z"    -- 2
  , "ps"   -- 3
  , "st"   -- 4
  , "cp"   -- 5
  , "ns"   -- 6
  , "čk"   -- 7
  , "lẓ"   -- 8
  , "pc"   -- 9
  ]

-- | Teens (10-19)
teenRoots :: [Text]
teenRoots =
  [ "j"     -- 10
  , "jks"   -- 11
  , "jz"    -- 12
  , "jps"   -- 13
  , "jst"   -- 14
  , "jcp"   -- 15
  , "jns"   -- 16
  , "jčk"   -- 17
  , "jlẓ"   -- 18
  , "jpc"   -- 19
  ]

-- | Tens (20, 30, ..., 90)
tensRoots :: [Text]
tensRoots =
  [ ""      -- 0 (not used directly)
  , ""      -- 10 (handled by teens)
  , "vz"    -- 20
  , "vps"   -- 30
  , "vst"   -- 40
  , "vcp"   -- 50
  , "vns"   -- 60
  , "včk"   -- 70
  , "vlẓ"   -- 80
  , "vpc"   -- 90
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

-- | Affixes for powers of 100
powerAffixes :: [Text]
powerAffixes =
  [ ""      -- 100^0 = 1
  , "çk"    -- 100^1 = 100 (hundred)
  , "çp"    -- 100^2 = 10,000 (myriad)
  , "çt"    -- 100^3 = 1,000,000 (million)
  , "çn"    -- 100^4 = 100,000,000
  , "çm"    -- 100^5 = 10,000,000,000
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
parseTensOnes _ =
  -- Try to match tens + ones pattern
  Nothing  -- Simplified; full implementation would parse compound
