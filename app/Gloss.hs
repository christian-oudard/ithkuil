{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Glossing Tool
-- Parses and glosses Ithkuil words and sentences
module Main where

import Control.Monad (when)
import System.Environment (getArgs)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..), stdin, hIsTerminalDevice, hIsEOF)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Ithkuil.Grammar
import Ithkuil.Parse (ParsedFormative(..), ParsedCa(..))
import Ithkuil.Referentials (PersonalRef(..), ReferentEffect(..), referentLabel)
import Ithkuil.WordType
import Ithkuil.Lexicon

main :: IO ()
main = do
  args <- getArgs
  roots <- loadLexicon "data/roots.json"
  affixes <- loadAffixLexicon "data/affixes.json"
  case args of
    [] -> do
      isTerm <- hIsTerminalDevice stdin
      if isTerm
        then repl roots affixes
        else pipeMode roots affixes
    ws -> do
      let sentence = T.pack (unwords ws)
      glossLine roots affixes sentence

loadLexicon :: FilePath -> IO (Map.Map Text RootEntry)
loadLexicon path = do
  result <- loadRoots path
  case result of
    Left _ -> return Map.empty
    Right r -> return r

loadAffixLexicon :: FilePath -> IO (Map.Map Text AffixEntry)
loadAffixLexicon path = do
  result <- loadAffixes path
  case result of
    Left _ -> return Map.empty
    Right a -> return a

repl :: Map.Map Text RootEntry -> Map.Map Text AffixEntry -> IO ()
repl roots affixes = do
  hSetBuffering stdout LineBuffering
  TIO.putStrLn $ "Ithkuil V4 Glosser (" <> T.pack (show (Map.size roots))
               <> " roots, " <> T.pack (show (Map.size affixes)) <> " affixes)"
  TIO.putStrLn "Enter Ithkuil text (Ctrl-D to quit):"
  loop
  where
    loop = do
      TIO.putStr "> "
      hFlush stdout
      eof <- System.IO.hIsEOF stdin
      if eof
        then return ()
        else do
          line <- TIO.getLine
          if T.null (T.strip line)
            then loop
            else do
              glossLine roots affixes (T.strip line)
              TIO.putStrLn ""
              loop

pipeMode :: Map.Map Text RootEntry -> Map.Map Text AffixEntry -> IO ()
pipeMode roots affixes = do
  contents <- TIO.getContents
  mapM_ (glossLine roots affixes . T.strip) (filter (not . T.null . T.strip) (T.lines contents))

glossLine :: Map.Map Text RootEntry -> Map.Map Text AffixEntry -> Text -> IO ()
glossLine roots affixes input = do
  let ws = T.words input
  mapM_ (glossOneWord roots affixes) ws
  -- Show interlinear summary with compact glosses
  when (length ws > 1) $ do
    let glosses = map (\w -> glossWordCompact roots affixes (parseWord w)) ws
        widths = zipWith (\w g -> max (T.length w) (T.length g) + 2) ws glosses
        padTo n t = t <> T.replicate (max 0 (n - T.length t)) " "
    TIO.putStrLn ""
    TIO.putStr "  "
    mapM_ (\(w, n) -> TIO.putStr (padTo n w)) (zip ws widths)
    TIO.putStrLn ""
    TIO.putStr "  "
    mapM_ (\(g, n) -> TIO.putStr (padTo n g)) (zip glosses widths)
    TIO.putStrLn ""

glossOneWord :: Map.Map Text RootEntry -> Map.Map Text AffixEntry -> Text -> IO ()
glossOneWord roots affixes word = do
  let wtype = classifyWord word
      parsed = parseWord word
  TIO.putStrLn $ "  " <> word <> "  [" <> T.pack (show wtype) <> "]"
  case parsed of
    PFormative pf -> showFormativeDetail roots affixes pf
    PBias b -> TIO.putStrLn $ "    Bias: " <> T.pack (show b)
    PRegister r -> TIO.putStrLn $ "    Register: " <> T.pack (show r)
    PReferential ref mc vc -> showReferentialDetail ref mc vc
    PModular pairs _raw -> do
      mapM_ (\s8 -> TIO.putStrLn $ "    VnCn: " <> glossSlotVIII s8) pairs
    PAffixual cs deg _ -> TIO.putStrLn $ "    Affix: " <> cs <> " degree " <> T.pack (show deg)
    PCarrier ct content -> TIO.putStrLn $ "    Carrier: " <> T.pack (show ct) <> " " <> content
    PUnparsed _ -> TIO.putStrLn $ "    (unparsed)"
  TIO.putStrLn $ "    GLOSS: " <> glossWord roots affixes parsed

showFormativeDetail :: Map.Map Text RootEntry -> Map.Map Text AffixEntry -> ParsedFormative -> IO ()
showFormativeDetail roots affixes pf = do
  let Root cr = pfRoot pf
      (stem, ver) = pfSlotII pf
      (func, spec, ctx) = pfSlotIV pf
      _conjs = pfConjuncts pf
  -- Root
  let rootMeaning = case lookupRoot cr roots of
        Just entry -> selectStem stem entry
        Nothing -> "(not in lexicon)"
  TIO.putStrLn $ "    Root: -" <> cr <> "- = " <> rootMeaning
  TIO.putStrLn $ "    Stem/Version: " <> T.pack (show stem) <> "/" <> T.pack (show ver)
  TIO.putStrLn $ "    Function/Spec/Context: " <> T.pack (show func) <> "/"
               <> T.pack (show spec) <> "/" <> T.pack (show ctx)
  -- Ca
  case pfCaParsed pf of
    Just pc | pc /= ParsedCa UNI CSL M_ DEL NRM ->
      TIO.putStrLn $ "    Ca: " <> showCaDetail pc
    _ -> return ()
  -- Affixes
  let afxPairs = extractAffixes (pfCa pf)
  mapM_ (\(vx, cs) -> do
    let degree = classifyDegree vx
        desc = case lookupAffix cs affixes of
          Just entry -> affixAbbrev entry <> " (" <> affixDesc entry <> ")"
                     <> " deg " <> T.pack (show degree)
                     <> case safeIndex (affixDegrees entry) (degree - 1) of
                          Just meaning -> ": " <> meaning
                          Nothing -> ""
          Nothing -> cs <> " deg " <> T.pack (show degree)
    TIO.putStrLn $ "    Affix: -" <> cs <> "- " <> vx <> " = " <> desc
    ) afxPairs
  -- Slot IX: Case or Illocution+Validation
  case pfIllocVal pf of
    Just (ill, val) -> TIO.putStrLn $ "    Illocution/Validation: "
                     <> T.pack (show ill) <> "/" <> T.pack (show val)
    Nothing -> case pfCase pf of
      Just c -> TIO.putStrLn $ "    Case: " <> T.pack (showCaseDetail c)
      Nothing -> return ()
  -- Stress
  let stress = pfStress pf
  if stress /= Penultimate
    then TIO.putStrLn $ "    Stress: " <> T.pack (show stress)
    else return ()

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i >= 0 && i < length xs = Just (xs !! i)
  | otherwise = Nothing

selectStem :: Stem -> RootEntry -> Text
selectStem S0 = rootStem0
selectStem S1 = rootStem1
selectStem S2 = rootStem2
selectStem S3 = rootStem3

showCaDetail :: ParsedCa -> Text
showCaDetail pc = T.intercalate "/" $ filter (not . T.null)
  [ if pcConfig pc /= UNI then T.pack (show (pcConfig pc)) else ""
  , if pcAffiliation pc /= CSL then T.pack (show (pcAffiliation pc)) else ""
  , if pcPerspective pc /= M_ then showPersp (pcPerspective pc) else ""
  , if pcExtension pc /= DEL then T.pack (show (pcExtension pc)) else ""
  , if pcEssence pc /= NRM then T.pack (show (pcEssence pc)) else ""
  ]
  where
    showPersp M_ = "M"
    showPersp G_ = "G"
    showPersp N_ = "N"
    showPersp A_ = "A"

showCaseDetail :: Case -> String
showCaseDetail (Transrelative c) = show c
showCaseDetail (Appositive c) = show c
showCaseDetail (Associative c) = show c
showCaseDetail (Adverbial c) = show c
showCaseDetail (Relational c) = show c
showCaseDetail (Affinitive c) = show c
showCaseDetail (SpatioTemporal1 c) = show c
showCaseDetail (SpatioTemporal2 c) = show c

showReferentialDetail :: PersonalRef -> Maybe Case -> Text -> IO ()
showReferentialDetail (PersonalRef ref eff) mc vc = do
  TIO.putStrLn $ "    Referent: " <> T.pack (show ref) <> " = " <> referentLabel ref
               <> (if eff /= NEU then " /" <> T.pack (show eff) else "")
  case mc of
    Just c -> TIO.putStrLn $ "    Case: " <> T.pack (showCaseDetail c)
    Nothing -> TIO.putStrLn $ "    Case vowel: " <> vc <> " (unrecognized)"
