{-# LANGUAGE OverloadedStrings #-}
-- | Gloss the word "Ithkuil" using real parsing and lexicon data
-- No hardcoded output - everything generated from code

module Main where

import Ithkuil.Parse
import Ithkuil.Grammar
import Ithkuil.Lexicon (RootEntry(..), V1RootEntry(..), loadRoots, loadV1Roots, lookupRoot, lookupV1Root)
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
  -- Load V4 lexicon
  rootsResult <- loadRoots "data/roots.json"
  roots <- case rootsResult of
    Left err -> putStrLn ("Failed to load V4 roots: " ++ err) >> exitFailure
    Right r -> return r

  -- Load V3 lexicon
  v3RootsResult <- loadRoots "data/roots_v3.json"
  v3Roots <- case v3RootsResult of
    Left err -> putStrLn ("Note: V3 lexicon not loaded: " ++ err) >> return Map.empty
    Right r -> return r

  -- Load V1 lexicon (biliteral roots)
  v1RootsResult <- loadV1Roots "data/roots_v1.json"
  v1Roots <- case v1RootsResult of
    Left err -> putStrLn ("Note: V1 lexicon not loaded: " ++ err) >> return Map.empty
    Right r -> return r

  TIO.putStrLn $ "Lexicon: " <> T.pack (show (Map.size roots)) <> " V4 roots, "
               <> T.pack (show (Map.size v3Roots)) <> " V3 roots, "
               <> T.pack (show (Map.size v1Roots)) <> " V1 roots loaded"
  putStrLn ""

  -- Parse Malëuţřait (V4)
  let word = "malëuţřait" :: Text
  TIO.putStrLn $ "═══════════════════════════════════════════"
  TIO.putStrLn $ "  ITHKUIL IV: " <> word
  TIO.putStrLn $ "═══════════════════════════════════════════"
  putStrLn ""

  glossV4 roots word

  -- Summary table
  putStrLn ""
  putStrLn "═══════════════════════════════════════════"
  putStrLn "  ALL FOUR VERSIONS"
  putStrLn "═══════════════════════════════════════════"
  putStrLn ""
  putStrLn "┌─────────┬────────────┬───────┬─────────────────────────────────────┐"
  putStrLn "│ Version │ Word       │ Root  │ Meaning                             │"
  putStrLn "├─────────┼────────────┼───────┼─────────────────────────────────────┤"

  -- V4 row
  case parseFormativeReal "malëuţřait" of
    Nothing -> putStrLn "│ IV      │ Malëuţřait │ -?-   │ (parse failed)                      │"
    Just pf -> do
      let Root cr = pfRoot pf
          (stem, _) = pfSlotII pf
      case lookupRoot cr roots of
        Nothing -> TIO.putStrLn $ "│ IV      │ Malëuţřait │ -" <> cr <> "-   │ (not in lexicon)                    │"
        Just entry -> do
          let meaning = selectStem stem entry
          TIO.putStrLn $ "│ IV      │ Malëuţřait │ -" <> cr <> "-   │ " <> padTo 35 meaning <> " │"

  -- V3 row - use V3 parser with V3 lexicon
  case V3.parseV3Formative "elartkha" of
    Nothing -> putStrLn "│ III     │ Elartkha   │ -?-   │ (parse failed)                      │"
    Just v3f -> do
      let V3.Root cr = V3.v3fRoot v3f
      -- V3 uses root -L- (language/speech) - use V3 lexicon
      case lookupRoot cr v3Roots of
        Nothing -> TIO.putStrLn $ "│ III     │ Elartkha   │ -" <> cr <> "-   │ (not in V3 lexicon)                 │"
        Just entry -> do
          let (_, stem, _) = V3.v3fSlotII v3f
          let meaning = selectStem stem entry
          TIO.putStrLn $ "│ III     │ Elartkha   │ -" <> cr <> "-   │ " <> padTo 35 meaning <> " │"

  -- V2 row - use V2 parser with V3 lexicon (same root -L-)
  case V2.parseV2Formative "ilákš" of
    Nothing -> putStrLn "│ II      │ ilákš      │ -?-   │ (parse failed)                      │"
    Just v2f -> do
      let V2.Root cr = V2.v2fRoot v2f
      case lookupRoot cr v3Roots of
        Nothing -> TIO.putStrLn $ "│ II      │ ilákš      │ -" <> cr <> "-   │ (not in V3 lexicon)                 │"
        Just entry -> do
          let meaning = selectStem (V2.v2fStem v2f) entry
          TIO.putStrLn $ "│ II      │ ilákš      │ -" <> cr <> "-   │ " <> padTo 35 meaning <> " │"

  -- V1 row - use V1 parser (biliteral root -K-L-) with V1 lexicon
  case V1.parseV1Formative "iţkuîl" of
    Nothing -> putStrLn "│ I       │ Iţkuîl     │ -?-?- │ (parse failed)                      │"
    Just v1f -> do
      let V1.V1Root c1 c2 = V1.v1fRoot v1f
          rootDisplay = "-" <> c1 <> "-" <> c2 <> "-"
      case lookupV1Root c1 c2 v1Roots of
        Nothing -> TIO.putStrLn $ "│ I       │ Iţkuîl     │ " <> padTo 5 rootDisplay <> " │ (not in V1 lexicon)                 │"
        Just entry -> do
          -- V1 uses stem based on mode - Secondary mode = stem2 (imaginary language)
          let meaning = case V1.v1fMode v1f of
                V1.Secondary -> v1Stem2 entry
                V1.Primary -> v1Stem0 entry
          TIO.putStrLn $ "│ I       │ Iţkuîl     │ " <> padTo 5 rootDisplay <> " │ " <> padTo 35 meaning <> " │"
  putStrLn "└─────────┴────────────┴───────┴─────────────────────────────────────┘"

-- | Gloss a V4 word with detailed slot analysis
glossV4 :: Map.Map Text RootEntry -> Text -> IO ()
glossV4 roots word = do
  let conjs = splitConjuncts word

  -- Show conjuncts with proper Unicode
  TIO.putStrLn $ "Conjuncts: " <> T.intercalate " - " conjs
  putStrLn ""

  case parseFormativeReal word of
    Nothing -> putStrLn "Parse failed!"
    Just pf -> do
      let Root cr = pfRoot pf
          (stem, version) = pfSlotII pf
          (func, spec, ctx) = pfSlotIV pf

      -- Find the Vr vowel from conjuncts
      let vrVowel = case conjs of
            (_:vr:_) -> vr
            _ -> "?"

      putStrLn "┌──────────┬────────┬─────────────────┬──────────────────────────────────┐"
      putStrLn "│ Slot     │ Form   │ Value           │ Meaning                          │"
      putStrLn "├──────────┼────────┼─────────────────┼──────────────────────────────────┤"

      -- Slot II (Vv) - elided
      TIO.putStrLn $ "│ II (Vv)  │ (a)    │ " <> padTo 15 (showSlotII stem version) <> " │ Stem+Version (elided default)    │"

      -- Slot III (Cr) - root
      case lookupRoot cr roots of
        Nothing -> TIO.putStrLn $ "│ III (Cr) │ " <> padTo 6 cr <> " │ Root            │ (not found in lexicon)           │"
        Just entry -> do
          let meaning = selectStem stem entry
          TIO.putStrLn $ "│ III (Cr) │ " <> padTo 6 cr <> " │ Root -" <> cr <> "-" <> T.replicate (7 - T.length cr) " " <> " │ " <> padTo 32 meaning <> " │"

      -- Slot IV (Vr)
      TIO.putStrLn $ "│ IV (Vr)  │ " <> padTo 6 vrVowel <> " │ " <> padTo 15 (showSlotIV func spec ctx) <> " │ Function/Specification/Context   │"

      -- Ca complex
      let caText = T.intercalate "+" (pfCa pf)
          caValue = case pfCaParsed pf of
            Nothing -> "(unparsed)"
            Just pc -> showCa pc
      TIO.putStrLn $ "│ VI (Ca)  │ " <> padTo 6 caText <> " │ " <> padTo 15 caValue <> " │ Config/Affil/Persp/Ext/Ess       │"

      -- Case - find case vowel (last vowel before any trailing consonants)
      let findCaseVowel [] = "?"
          findCaseVowel (x:xs)
            | not (T.null x) && isVowelChar (T.head x) = x
            | otherwise = findCaseVowel xs
          vcVowel = findCaseVowel (reverse conjs)
      case pfCase pf of
        Nothing -> TIO.putStrLn $ "│ IX (Vc)  │ " <> padTo 6 vcVowel <> " │ (not parsed)    │ Case                             │"
        Just c -> TIO.putStrLn $ "│ IX (Vc)  │ " <> padTo 6 vcVowel <> " │ " <> padTo 15 (T.pack (show c)) <> " │ Case                             │"

      putStrLn "└──────────┴────────┴─────────────────┴──────────────────────────────────┘"

      -- Gloss line
      putStrLn ""
      TIO.putStrLn $ "GLOSS: " <> buildGloss pf roots

-- | Select stem meaning from entry
selectStem :: Stem -> RootEntry -> Text
selectStem S0 = rootStem0
selectStem S1 = rootStem1
selectStem S2 = rootStem2
selectStem S3 = rootStem3

-- | Build gloss string
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

-- | Format Slot II
showSlotII :: Stem -> Version -> Text
showSlotII stem ver = T.pack (showStem stem) <> "/" <> T.pack (showVersion ver)

-- | Format Slot IV
showSlotIV :: Function -> Specification -> Context -> Text
showSlotIV f s c = T.pack (showFunc f) <> "/" <> T.pack (showSpec s) <> "/" <> T.pack (showCtx c)

-- | Pad text to width
padTo :: Int -> Text -> Text
padTo n t = t <> T.replicate (max 0 (n - T.length t)) " "

-- Helper functions
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

-- | Show parsed Ca complex
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
