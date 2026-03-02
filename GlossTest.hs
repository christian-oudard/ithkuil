{-# LANGUAGE OverloadedStrings #-}
module Main where

import Ithkuil.Grammar
import Ithkuil.Parse (parseFormativeReal, ParsedFormative(..), ParsedCa(..), splitConjuncts, isVowelChar)
import Ithkuil.FullParse (ParseResult(..), parseFormative)
import Ithkuil.Gloss
import Ithkuil.Lexicon
import qualified Ithkuil.V3.Parse as V3
import qualified Ithkuil.V3.Grammar as V3
import qualified Ithkuil.V2.Parse as V2
import qualified Ithkuil.V2.Grammar as V2
import qualified Ithkuil.V1.Parse as V1
import qualified Ithkuil.V1.Grammar as V1
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)

main :: IO ()
main = do
  -- Load lexicons
  roots <- loadOrDie "data/roots.json" loadRoots
  affixes <- loadOrEmpty "data/affixes.json" loadAffixes
  v3Roots <- loadOrEmpty "data/roots_v3.json" loadRoots
  v1Roots <- loadOrEmpty "data/roots_v1.json" loadV1Roots

  TIO.putStrLn $ "Lexicon: " <> showN (Map.size roots) <> " V4 roots, "
    <> showN (Map.size affixes) <> " affixes, "
    <> showN (Map.size v3Roots) <> " V3 roots, "
    <> showN (Map.size v1Roots) <> " V1 roots"
  putStrLn ""

  -- Demonstrate glossing at all 3 precision levels
  let word = "malëuţřait" :: Text
  section "GLOSSING: malëuţřait (Ithkuil IV)"

  TIO.putStrLn $ "Conjuncts: " <> T.intercalate " - " (splitConjuncts word)
  putStrLn ""

  for_ [Short, Regular, Full] $ \prec -> do
    let result = glossWord prec roots affixes word
    TIO.putStrLn $ padTo 9 (showPrec prec) <> grGloss result
  putStrLn ""

  -- Slot-by-slot detail
  let detail = glossWord Full roots affixes word
  TIO.putStrLn "Slot-by-slot analysis:"
  for_ (grDetails detail) $ \(slot, val) ->
    TIO.putStrLn $ "  " <> padTo 10 slot <> val
  putStrLn ""

  -- Full parse detail
  case parseFormative word of
    Success f -> do
      TIO.putStrLn "Full parse result:"
      TIO.putStrLn $ "  Stem:     " <> T.pack (show (fSlotII f))
      TIO.putStrLn $ "  Root:     " <> T.pack (show (fSlotIII f))
      TIO.putStrLn $ "  Func:     " <> T.pack (show (fSlotIV f))
      TIO.putStrLn $ "  Ca:       " <> T.pack (show (fSlotVI f))
      TIO.putStrLn $ "  Case:     " <> T.pack (show (fSlotIX f))
      TIO.putStrLn $ "  Stress:   " <> T.pack (show (fStress f))
    Failure e -> TIO.putStrLn $ "  Parse error: " <> e
  putStrLn ""

  -- Sentence glossing demo
  section "SENTENCE GLOSSING"
  let sentence = "malëuţřait" -- single word for now
  let results = glossSentence Regular roots affixes sentence
  for_ results $ \r ->
    TIO.putStrLn $ grWord r <> " = " <> grGloss r
  putStrLn ""

  -- Cross-version comparison table
  section "ALL FOUR VERSIONS"
  showVersionTable roots v3Roots v1Roots

-- | Cross-version comparison
showVersionTable :: Map.Map Text RootEntry -> Map.Map Text RootEntry -> Map.Map Text V1RootEntry -> IO ()
showVersionTable roots v3Roots v1Roots = do
  -- V4
  let v4 = case parseFormativeReal "malëuţřait" of
        Nothing -> ("(parse failed)", "?")
        Just pf -> let Root cr = pfRoot pf; (stem, _) = pfSlotII pf
                   in ("-" <> cr <> "-", maybe "?" (selectStem stem) (lookupRoot cr roots))

  -- V3
  let v3 = case V3.parseV3Formative "elartkha" of
        Nothing -> ("?", "(parse failed)")
        Just v3f -> let V3.Root cr = V3.v3fRoot v3f; (_, stem, _) = V3.v3fSlotII v3f
                    in ("-" <> cr <> "-", maybe "?" (selectStem stem) (lookupRoot cr v3Roots))

  -- V2
  let v2 = case V2.parseV2Formative "ilákš" of
        Nothing -> ("?", "(parse failed)")
        Just v2f -> let V2.Root cr = V2.v2fRoot v2f
                    in ("-" <> cr <> "-", maybe "?" (selectStem (V2.v2fStem v2f)) (lookupRoot cr v3Roots))

  -- V1
  let v1 = case V1.parseV1Formative "iţkuîl" of
        Nothing -> ("?", "(parse failed)")
        Just v1f -> let V1.V1Root c1 c2 = V1.v1fRoot v1f
                        meaning = case lookupV1Root c1 c2 v1Roots of
                          Nothing -> "?"
                          Just e -> case V1.v1fMode v1f of
                            V1.Secondary -> v1Stem2 e
                            V1.Primary -> v1Stem0 e
                    in ("-" <> c1 <> "-" <> c2 <> "-", meaning)

  TIO.putStrLn $ padTo 10 "Version" <> padTo 14 "Word" <> padTo 10 "Root" <> "Meaning"
  TIO.putStrLn $ T.replicate 70 "-"
  TIO.putStrLn $ padTo 10 "IV"  <> padTo 14 "Malëuţřait" <> padTo 10 (fst v4) <> snd v4
  TIO.putStrLn $ padTo 10 "III" <> padTo 14 "Elartkha"   <> padTo 10 (fst v3) <> snd v3
  TIO.putStrLn $ padTo 10 "II"  <> padTo 14 "ilákš"      <> padTo 10 (fst v2) <> snd v2
  TIO.putStrLn $ padTo 10 "I"   <> padTo 14 "Iţkuîl"     <> padTo 10 (fst v1) <> snd v1

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

selectStem :: Stem -> RootEntry -> Text
selectStem S0 = rootStem0
selectStem S1 = rootStem1
selectStem S2 = rootStem2
selectStem S3 = rootStem3

padTo :: Int -> Text -> Text
padTo n t = t <> T.replicate (max 0 (n - T.length t)) " "

showN :: Int -> Text
showN = T.pack . show

showPrec :: Precision -> Text
showPrec Short = "Short:  "
showPrec Regular = "Regular:"
showPrec Full = "Full:   "

section :: String -> IO ()
section title = do
  putStrLn $ replicate 50 '='
  putStrLn $ "  " ++ title
  putStrLn $ replicate 50 '='
  putStrLn ""

for_ :: [a] -> (a -> IO ()) -> IO ()
for_ = flip mapM_

loadOrDie :: FilePath -> (FilePath -> IO (Either String a)) -> IO a
loadOrDie path loader = do
  result <- loader path
  case result of
    Left err -> putStrLn ("FATAL: " ++ err) >> exitFailure
    Right a -> return a

loadOrEmpty :: (Monoid a) => FilePath -> (FilePath -> IO (Either String a)) -> IO a
loadOrEmpty path loader = do
  result <- loader path
  case result of
    Left _ -> return mempty
    Right a -> return a
