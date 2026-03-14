{-# LANGUAGE OverloadedStrings #-}
-- | Gloss the word "Malëuţřait" using real parsing and lexicon data

module Main where

import Ithkuil.Parse
import Ithkuil.Grammar
import Ithkuil.Lexicon (RootEntry(..), loadRoots, lookupRoot)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)

main :: IO ()
main = do
  rootsResult <- loadRoots "data/roots.json"
  roots <- case rootsResult of
    Left err -> putStrLn ("Failed to load roots: " ++ err) >> exitFailure
    Right r -> return r

  TIO.putStrLn $ "Lexicon: " <> T.pack (show (Map.size roots)) <> " roots loaded"
  putStrLn ""

  let word = "malëuţřait" :: Text
  TIO.putStrLn "═══════════════════════════════════════════"
  TIO.putStrLn $ "  ITHKUIL V4: " <> word
  TIO.putStrLn "═══════════════════════════════════════════"
  putStrLn ""

  glossV4 roots word

-- | Gloss a V4 word with detailed slot analysis
glossV4 :: Map.Map Text RootEntry -> Text -> IO ()
glossV4 roots word = do
  let conjs = splitConjuncts word

  TIO.putStrLn $ "Conjuncts: " <> T.intercalate " - " conjs
  putStrLn ""

  case parseFormativeReal word of
    Nothing -> putStrLn "Parse failed!"
    Just pf -> do
      let Root cr = pfRoot pf
          (stem, version) = pfSlotII pf
          (func, spec, ctx) = pfSlotIV pf

      let vrVowel = case conjs of
            (_:vr:_) -> vr
            _ -> "?"

      putStrLn "┌──────────┬────────┬─────────────────┬──────────────────────────────────┐"
      putStrLn "│ Slot     │ Form   │ Value           │ Meaning                          │"
      putStrLn "├──────────┼────────┼─────────────────┼──────────────────────────────────┤"

      TIO.putStrLn $ "│ II (Vv)  │ (a)    │ " <> padTo 15 (showSlotII stem version) <> " │ Stem+Version (elided default)    │"

      case lookupRoot cr roots of
        Nothing -> TIO.putStrLn $ "│ III (Cr) │ " <> padTo 6 cr <> " │ Root            │ (not found in lexicon)           │"
        Just entry -> do
          let meaning = selectStem stem entry
          TIO.putStrLn $ "│ III (Cr) │ " <> padTo 6 cr <> " │ Root -" <> cr <> "-" <> T.replicate (7 - T.length cr) " " <> " │ " <> padTo 32 meaning <> " │"

      TIO.putStrLn $ "│ IV (Vr)  │ " <> padTo 6 vrVowel <> " │ " <> padTo 15 (showSlotIV func spec ctx) <> " │ Function/Specification/Context   │"

      let caText = T.intercalate "+" (pfCa pf)
          caValue = case pfCaParsed pf of
            Nothing -> "(unparsed)"
            Just pc -> showCa pc
      TIO.putStrLn $ "│ VI (Ca)  │ " <> padTo 6 caText <> " │ " <> padTo 15 caValue <> " │ Config/Affil/Persp/Ext/Ess       │"

      let findCaseVowel [] = "?"
          findCaseVowel (x:xs)
            | not (T.null x) && isVowelChar (T.head x) = x
            | otherwise = findCaseVowel xs
          vcVowel = findCaseVowel (reverse conjs)
      case pfCase pf of
        Nothing -> TIO.putStrLn $ "│ IX (Vc)  │ " <> padTo 6 vcVowel <> " │ (not parsed)    │ Case                             │"
        Just c -> TIO.putStrLn $ "│ IX (Vc)  │ " <> padTo 6 vcVowel <> " │ " <> padTo 15 (T.pack (show c)) <> " │ Case                             │"

      putStrLn "└──────────┴────────┴─────────────────┴──────────────────────────────────┘"

      putStrLn ""
      TIO.putStrLn $ "GLOSS: " <> buildGloss pf roots

selectStem :: Stem -> RootEntry -> Text
selectStem S0 = rootStem0
selectStem S1 = rootStem1
selectStem S2 = rootStem2
selectStem S3 = rootStem3

buildGloss :: ParsedFormative -> Map.Map Text RootEntry -> Text
buildGloss pf roots =
  let Root cr = pfRoot pf
      (stem, version) = pfSlotII pf
      (func, spec, ctx) = pfSlotIV pf
      meaning = case lookupRoot cr roots of
        Nothing -> "?"
        Just e -> selectStem stem e
      caGloss = case pfCaParsed pf of
        Nothing -> ""
        Just pc -> "-" <> showCa pc
      caseGloss = case pfCase pf of
        Nothing -> ""
        Just c -> "-" <> T.pack (show c)
  in T.concat
    [ showSlotII stem version
    , "-", cr, ":'", meaning, "'"
    , "-", showSlotIV func spec ctx
    , caGloss
    , caseGloss
    ]

showSlotII :: Stem -> Version -> Text
showSlotII stem ver = T.pack (showStem stem) <> "/" <> T.pack (showVersion ver)

showSlotIV :: Function -> Specification -> Context -> Text
showSlotIV f s c = T.pack (showFunc f) <> "/" <> T.pack (showSpec s) <> "/" <> T.pack (showCtx c)

padTo :: Int -> Text -> Text
padTo n t = t <> T.replicate (max 0 (n - T.length t)) " "

showStem :: Stem -> String
showStem S0 = "S0"
showStem S1 = "S1"
showStem S2 = "S2"
showStem S3 = "S3"

showVersion :: Version -> String
showVersion PRC = "PRC"
showVersion CPT = "CPT"

showFunc :: Function -> String
showFunc STA = "STA"
showFunc DYN = "DYN"

showSpec :: Specification -> String
showSpec BSC = "BSC"
showSpec CTE = "CTE"
showSpec CSV = "CSV"
showSpec OBJ = "OBJ"

showCtx :: Context -> String
showCtx EXS = "EXS"
showCtx FNC = "FNC"
showCtx RPS = "RPS"
showCtx AMG = "AMG"

showCa :: ParsedCa -> Text
showCa pc = T.intercalate "/" $ map T.pack
  [ showConfig (pcConfig pc)
  , showAffil (pcAffiliation pc)
  , showPersp (pcPerspective pc)
  , showExt (pcExtension pc)
  , showEss (pcEssence pc)
  ]

showConfig :: Configuration -> String
showConfig UNI = "UNI"
showConfig DPX = "DPX"
showConfig DSS = "DSS"
showConfig DSC = "DSC"
showConfig DSF = "DSF"
showConfig DDS = "DDS"
showConfig DDC = "DDC"
showConfig DDF = "DDF"
showConfig DFS = "DFS"
showConfig DFC = "DFC"
showConfig DFF = "DFF"
showConfig MSS = "MSS"
showConfig MSC = "MSC"
showConfig MSF = "MSF"
showConfig MDS = "MDS"
showConfig MDC = "MDC"
showConfig MDF = "MDF"
showConfig MFS = "MFS"
showConfig MFC = "MFC"
showConfig MFF = "MFF"

showAffil :: Affiliation -> String
showAffil CSL = "CSL"
showAffil ASO = "ASO"
showAffil COA = "COA"
showAffil VAR = "VAR"

showPersp :: Perspective -> String
showPersp M_ = "M"
showPersp G_ = "G"
showPersp N_ = "N"
showPersp A_ = "A"

showExt :: Extension -> String
showExt DEL = "DEL"
showExt PRX = "PRX"
showExt ICP = "ICP"
showExt ATV = "ATV"
showExt GRA = "GRA"
showExt DPL = "DPL"

showEss :: Essence -> String
showEss NRM = "NRM"
showEss RPV = "RPV"
