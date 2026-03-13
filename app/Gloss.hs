{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Glossing Tool
-- Parses and glosses Ithkuil words and sentences
module Main where

import Control.Monad (when, forM_)
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
import Ithkuil.Compose (lookupGrammar, GrammarEntry(..), searchRoots, searchAffixes, dumpGrammarTable, composeFormative)
import Ithkuil.Script (renderFormativeSvg)

-- ANSI color helpers (only used when outputting to terminal)
dim, cyan, green, yellow, magenta, bold, reset :: Text
dim     = "\ESC[2m"
cyan    = "\ESC[36m"
green   = "\ESC[32m"
yellow  = "\ESC[33m"
magenta = "\ESC[35m"
bold    = "\ESC[1m"
reset   = "\ESC[0m"

-- | Wrap text in color, auto-resetting
col :: Text -> Text -> Text
col c t = c <> t <> reset


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> showHelp
    ["-h"] -> showHelp
    ("--lookup":rest) -> handleLookup rest
    ("--root":rest) -> handleRootSearch rest
    ("--affix":rest) -> handleAffixSearch rest
    ("--grammar":rest) -> handleGrammarDump rest
    ("--script":rest) -> handleScript rest
    ("--compose":rest) -> handleCompose rest
    _ -> do
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

handleLookup :: [String] -> IO ()
handleLookup [] = TIO.putStrLn "Usage: ithkuil-gloss --lookup <abbreviation>"
handleLookup (abbr:_) = do
  let results = lookupGrammar (T.pack abbr)
  case results of
    [] -> TIO.putStrLn $ "No grammar entry found for: " <> T.pack abbr
    _ -> mapM_ (\e ->
      TIO.putStrLn $ gAbbrev e <> "  " <> gName e
                  <> "  [" <> gCategory e <> "]"
                  <> "  form: " <> gForm e) results

handleRootSearch :: [String] -> IO ()
handleRootSearch [] = TIO.putStrLn "Usage: ithkuil-gloss --root <keyword>"
handleRootSearch ws = do
  roots <- loadLexicon "data/roots.json"
  let query = T.pack (unwords ws)
      results = searchRoots query roots
  if null results
    then TIO.putStrLn $ "No roots found matching: " <> query
    else mapM_ (\(cr, entry) ->
      TIO.putStrLn $ "-" <> cr <> "-  S1: " <> rootStem1 entry
                  <> "  S2: " <> rootStem2 entry
                  <> "  S3: " <> rootStem3 entry
                  <> "  S0: " <> rootStem0 entry) results

handleAffixSearch :: [String] -> IO ()
handleAffixSearch [] = TIO.putStrLn "Usage: ithkuil-gloss --affix <keyword>"
handleAffixSearch ws = do
  affixes <- loadAffixLexicon "data/affixes.json"
  let query = T.pack (unwords ws)
      results = searchAffixes query affixes
  if null results
    then TIO.putStrLn $ "No affixes found matching: " <> query
    else mapM_ (\(cs, entry) ->
      TIO.putStrLn $ "-" <> cs <> "-  " <> affixAbbrev entry
                  <> "  (" <> affixDesc entry <> ")") results

handleGrammarDump :: [String] -> IO ()
handleGrammarDump cats = do
  let category = T.pack (unwords cats)
  TIO.putStr (dumpGrammarTable category)

showHelp :: IO ()
showHelp = do
  TIO.putStrLn $ col bold "ithkuil-gloss" <> " - Ithkuil V4 parser and glosser"
  TIO.putStrLn ""
  TIO.putStrLn $ col bold "USAGE:"
  TIO.putStrLn "  ithkuil-gloss                   Interactive REPL mode"
  TIO.putStrLn "  ithkuil-gloss <words...>         Parse and gloss a sentence"
  TIO.putStrLn "  echo text | ithkuil-gloss        Pipe mode (one sentence per line)"
  TIO.putStrLn ""
  TIO.putStrLn $ col bold "COMMANDS:"
  TIO.putStrLn "  --lookup <abbr>     Look up grammar abbreviation (e.g. THM, LOC, SUB)"
  TIO.putStrLn "  --root <keyword>    Search roots by meaning keyword"
  TIO.putStrLn "  --affix <keyword>   Search affixes by keyword"
  TIO.putStrLn "  --grammar <cat>     Dump all entries in a grammar category"
  TIO.putStrLn "  --script <word>     Render a formative as SVG script"
  TIO.putStrLn "  --compose <root> [opts]  Compose a formative from grammar specs"
  TIO.putStrLn "    opts: S1-S3 DYN STA BSC CTE CSV OBJ ABS ERG DAT ALL LOC IRG DIR OBS etc."
  TIO.putStrLn "  --help, -h          Show this help"

handleScript :: [String] -> IO ()
handleScript [] = TIO.putStrLn "Usage: ithkuil-gloss --script <word>"
handleScript ws = do
  let word = T.pack (unwords ws)
      parsed = parseWord word
  case parsed of
    PFormative pf -> TIO.putStrLn (renderFormativeSvg pf)
    PConcatenated (pf:_) -> TIO.putStrLn (renderFormativeSvg pf)
    _ -> TIO.putStrLn $ "Cannot render script for non-formative: " <> word

handleCompose :: [String] -> IO ()
handleCompose [] = TIO.putStrLn "Usage: ithkuil-gloss --compose <root> [S1-S3] [DYN] [ABS|ERG|DAT|ALL|...] [IRG|DIR|...]"
handleCompose (rootStr:opts) = do
  roots <- loadLexicon "data/roots.json"
  affixes <- loadAffixLexicon "data/affixes.json"
  let root = T.pack rootStr
      flags = map (T.toUpper . T.pack) opts
      f0 = minimalFormative root
      f1 = applyComposeFlags flags f0
      f2 = autoStress f1
      word = composeFormative f2
  TIO.putStrLn $ col bold word
  glossLine roots affixes word

-- | Apply a list of grammar flags to a formative
applyComposeFlags :: [Text] -> Formative -> Formative
applyComposeFlags [] f = f
applyComposeFlags (flag:rest) f = applyComposeFlags rest (applyOneFlag flag f)

-- | Apply a single grammar flag to a formative
applyOneFlag :: Text -> Formative -> Formative
-- Stem/Version (Slot II)
applyOneFlag "S1" f = f { fSlotII = (S1, snd (fSlotII f)) }
applyOneFlag "S2" f = f { fSlotII = (S2, snd (fSlotII f)) }
applyOneFlag "S3" f = f { fSlotII = (S3, snd (fSlotII f)) }
applyOneFlag "S0" f = f { fSlotII = (S0, snd (fSlotII f)) }
applyOneFlag "PRC" f = f { fSlotII = (fst (fSlotII f), PRC) }
applyOneFlag "CPT" f = f { fSlotII = (fst (fSlotII f), CPT) }
-- Function/Specification/Context (Slot IV)
applyOneFlag "DYN" f = f { fSlotIV = (DYN, sel2 (fSlotIV f), sel3 (fSlotIV f)) }
applyOneFlag "STA" f = f { fSlotIV = (STA, sel2 (fSlotIV f), sel3 (fSlotIV f)) }
applyOneFlag "BSC" f = f { fSlotIV = (sel1 (fSlotIV f), BSC, sel3 (fSlotIV f)) }
applyOneFlag "CTE" f = f { fSlotIV = (sel1 (fSlotIV f), CTE, sel3 (fSlotIV f)) }
applyOneFlag "CSV" f = f { fSlotIV = (sel1 (fSlotIV f), CSV, sel3 (fSlotIV f)) }
applyOneFlag "OBJ" f = f { fSlotIV = (sel1 (fSlotIV f), OBJ, sel3 (fSlotIV f)) }
applyOneFlag "EXS" f = f { fSlotIV = (sel1 (fSlotIV f), sel2 (fSlotIV f), EXS) }
applyOneFlag "FNC" f = f { fSlotIV = (sel1 (fSlotIV f), sel2 (fSlotIV f), FNC) }
applyOneFlag "RPS" f = f { fSlotIV = (sel1 (fSlotIV f), sel2 (fSlotIV f), RPS) }
applyOneFlag "AMG" f = f { fSlotIV = (sel1 (fSlotIV f), sel2 (fSlotIV f), AMG) }
-- Configuration (Ca component 1)
applyOneFlag "UNI" f = setCa1 UNI f
applyOneFlag "DPX" f = setCa1 DPX f
applyOneFlag "DSS" f = setCa1 DSS f
applyOneFlag "DSC" f = setCa1 DSC f
applyOneFlag "DSF" f = setCa1 DSF f
applyOneFlag "MSS" f = setCa1 MSS f
applyOneFlag "MSC" f = setCa1 MSC f
applyOneFlag "MSF" f = setCa1 MSF f
applyOneFlag "MDS" f = setCa1 MDS f
applyOneFlag "MDC" f = setCa1 MDC f
applyOneFlag "MDF" f = setCa1 MDF f
applyOneFlag "MFS" f = setCa1 MFS f
applyOneFlag "MFC" f = setCa1 MFC f
applyOneFlag "MFF" f = setCa1 MFF f
applyOneFlag "DDS" f = setCa1 DDS f
applyOneFlag "DDC" f = setCa1 DDC f
applyOneFlag "DDF" f = setCa1 DDF f
applyOneFlag "DFS" f = setCa1 DFS f
applyOneFlag "DFC" f = setCa1 DFC f
applyOneFlag "DFF" f = setCa1 DFF f
-- Affiliation (Ca component 3)
applyOneFlag "CSL" f = setCa3 CSL f
applyOneFlag "ASO" f = setCa3 ASO f
applyOneFlag "COA" f = setCa3 COA f
applyOneFlag "VAR" f = setCa3 VAR f
-- Perspective (Ca component 4)
applyOneFlag "M" f = setCa4p M_ f
applyOneFlag "G" f = setCa4p G_ f
applyOneFlag "N" f = setCa4p N_ f
applyOneFlag "A" f = setCa4p A_ f
-- Extension (Ca component 2)
applyOneFlag "DEL" f = setCa2 DEL f
applyOneFlag "PRX" f = setCa2 PRX f
applyOneFlag "ICP" f = setCa2 ICP f
applyOneFlag "ATV" f = setCa2 ATV f
applyOneFlag "GRA" f = setCa2 GRA f
applyOneFlag "DPL" f = setCa2 DPL f
-- Essence (Ca component 5)
applyOneFlag "NRM" f = setCa5 NRM f
applyOneFlag "RPV" f = setCa5 RPV f
-- Transrelative Cases
applyOneFlag "THM" f = f { fSlotIX = Left (Transrelative THM) }
applyOneFlag "ABS" f = f { fSlotIX = Left (Transrelative ABS) }
applyOneFlag "ERG" f = f { fSlotIX = Left (Transrelative ERG) }
applyOneFlag "DAT" f = f { fSlotIX = Left (Transrelative DAT) }
applyOneFlag "IND" f = f { fSlotIX = Left (Transrelative IND) }
applyOneFlag "AFF" f = f { fSlotIX = Left (Transrelative AFF) }
applyOneFlag "STM" f = f { fSlotIX = Left (Transrelative STM) }
applyOneFlag "EFF" f = f { fSlotIX = Left (Transrelative EFF) }
applyOneFlag "INS" f = f { fSlotIX = Left (Transrelative INS) }
-- Appositive Cases
applyOneFlag "POS" f = f { fSlotIX = Left (Appositive POS) }
applyOneFlag "PRP" f = f { fSlotIX = Left (Appositive PRP) }
applyOneFlag "GEN" f = f { fSlotIX = Left (Appositive GEN) }
applyOneFlag "ATT" f = f { fSlotIX = Left (Appositive ATT) }
applyOneFlag "PDC" f = f { fSlotIX = Left (Appositive PDC) }
applyOneFlag "ITP" f = f { fSlotIX = Left (Appositive ITP) }
applyOneFlag "OGN" f = f { fSlotIX = Left (Appositive OGN) }
applyOneFlag "IDP" f = f { fSlotIX = Left (Appositive IDP) }
applyOneFlag "PAR" f = f { fSlotIX = Left (Appositive PAR) }
-- Spatio-Temporal Cases
applyOneFlag "LOC" f = f { fSlotIX = Left (SpatioTemporal1 LOC) }
applyOneFlag "ATD" f = f { fSlotIX = Left (SpatioTemporal1 ATD) }
applyOneFlag "ALL" f = f { fSlotIX = Left (SpatioTemporal1 ALL) }
applyOneFlag "ABL" f = f { fSlotIX = Left (SpatioTemporal1 ABL) }
applyOneFlag "ORI" f = f { fSlotIX = Left (SpatioTemporal1 ORI) }
applyOneFlag "IRL" f = f { fSlotIX = Left (SpatioTemporal1 IRL) }
applyOneFlag "INV" f = f { fSlotIX = Left (SpatioTemporal1 INV) }
applyOneFlag "NAV" f = f { fSlotIX = Left (SpatioTemporal1 NAV) }
applyOneFlag "CNR" f = f { fSlotIX = Left (SpatioTemporal2 CNR) }
applyOneFlag "PER" f = f { fSlotIX = Left (SpatioTemporal2 PER) }
applyOneFlag "PRO" f = f { fSlotIX = Left (SpatioTemporal2 PRO) }
applyOneFlag "ELP" f = f { fSlotIX = Left (SpatioTemporal2 ELP) }
applyOneFlag "PLM" f = f { fSlotIX = Left (SpatioTemporal2 PLM) }
-- Associative Cases
applyOneFlag "APL" f = f { fSlotIX = Left (Associative APL) }
applyOneFlag "PUR" f = f { fSlotIX = Left (Associative PUR) }
applyOneFlag "TRA" f = f { fSlotIX = Left (Associative TRA) }
applyOneFlag "DFR" f = f { fSlotIX = Left (Associative DFR) }
applyOneFlag "CRS" f = f { fSlotIX = Left (Associative CRS) }
applyOneFlag "TSP" f = f { fSlotIX = Left (Associative TSP) }
applyOneFlag "CMM" f = f { fSlotIX = Left (Associative CMM) }
applyOneFlag "CMP" f = f { fSlotIX = Left (Associative CMP) }
applyOneFlag "CSD" f = f { fSlotIX = Left (Associative CSD) }
-- Adverbial Cases
applyOneFlag "FUN" f = f { fSlotIX = Left (Adverbial FUN) }
applyOneFlag "TFM" f = f { fSlotIX = Left (Adverbial TFM) }
applyOneFlag "CLA" f = f { fSlotIX = Left (Adverbial CLA) }
applyOneFlag "RSL" f = f { fSlotIX = Left (Adverbial RSL) }
applyOneFlag "CSM" f = f { fSlotIX = Left (Adverbial CSM) }
applyOneFlag "CON" f = f { fSlotIX = Left (Adverbial CON) }
applyOneFlag "AVR" f = f { fSlotIX = Left (Adverbial AVR) }
applyOneFlag "CVS" f = f { fSlotIX = Left (Adverbial CVS) }
applyOneFlag "SIT" f = f { fSlotIX = Left (Adverbial SIT) }
-- Illocution+Validation (make verbal)
applyOneFlag "OBS" f = f { fSlotIX = Right (IllocVal ASR OBS) }
applyOneFlag "IRG" f = f { fSlotIX = Right (IllocVal IRG OBS) }
applyOneFlag "DIR" f = f { fSlotIX = Right (IllocVal DIR OBS) }
applyOneFlag "DEC" f = f { fSlotIX = Right (IllocVal DEC OBS) }
applyOneFlag "ADM" f = f { fSlotIX = Right (IllocVal ADM OBS) }
applyOneFlag "POT" f = f { fSlotIX = Right (IllocVal POT OBS) }
applyOneFlag "HOR" f = f { fSlotIX = Right (IllocVal HOR OBS) }
applyOneFlag "CNJ" f = f { fSlotIX = Right (IllocVal CNJ OBS) }
applyOneFlag "VER" f = f { fSlotIX = Right (IllocVal VER OBS) }
-- Validation (modifies existing illocution)
applyOneFlag "REC" f = setVal REC f
applyOneFlag "PUP" f = setVal PUP f
applyOneFlag "RPR" f = setVal RPR f
applyOneFlag "USP" f = setVal USP f
applyOneFlag "IMA" f = setVal IMA f
applyOneFlag "CVN" f = setVal CVN f
applyOneFlag "ITU" f = setVal ITU f
applyOneFlag "INF" f = setVal INF f
-- Aspect (Slot VIII, Pattern 2)
applyOneFlag "RTR" f = f { fSlotVIII = Just (VnCnAspect RTR (MoodVal FAC)) }
applyOneFlag "PRS" f = f { fSlotVIII = Just (VnCnAspect PRS (MoodVal FAC)) }
applyOneFlag "HAB" f = f { fSlotVIII = Just (VnCnAspect HAB (MoodVal FAC)) }
applyOneFlag "PRG" f = f { fSlotVIII = Just (VnCnAspect PRG (MoodVal FAC)) }
applyOneFlag "IMM" f = f { fSlotVIII = Just (VnCnAspect IMM (MoodVal FAC)) }
applyOneFlag "PCS" f = f { fSlotVIII = Just (VnCnAspect PCS (MoodVal FAC)) }
applyOneFlag "REG" f = f { fSlotVIII = Just (VnCnAspect REG (MoodVal FAC)) }
applyOneFlag "ATP" f = f { fSlotVIII = Just (VnCnAspect ATP (MoodVal FAC)) }
-- Framed relation (antepenultimate stress)
applyOneFlag "FRA" f = f { fStress = Antepenultimate }
applyOneFlag _ f = f  -- Ignore unknown flags

-- | Set validation on existing illocution, or default to ASR
setVal :: Validation -> Formative -> Formative
setVal v f = case fSlotIX f of
  Right (IllocVal ill _) -> f { fSlotIX = Right (IllocVal ill v) }
  _ -> f { fSlotIX = Right (IllocVal ASR v) }

-- Ca component setters
setCa1 :: Configuration -> Formative -> Formative
setCa1 c f = let (_, a, p, e, s) = fSlotVI f in f { fSlotVI = (c, a, p, e, s) }
setCa2 :: Extension -> Formative -> Formative
setCa2 e f = let (c, a, p, _, s) = fSlotVI f in f { fSlotVI = (c, a, p, e, s) }
setCa3 :: Affiliation -> Formative -> Formative
setCa3 a f = let (c, _, p, e, s) = fSlotVI f in f { fSlotVI = (c, a, p, e, s) }
setCa4p :: Perspective -> Formative -> Formative
setCa4p p f = let (c, a, _, e, s) = fSlotVI f in f { fSlotVI = (c, a, p, e, s) }
setCa5 :: Essence -> Formative -> Formative
setCa5 s f = let (c, a, p, e, _) = fSlotVI f in f { fSlotVI = (c, a, p, e, s) }

-- | Auto-determine stress from Slot IX, unless explicitly set (e.g. FRA)
autoStress :: Formative -> Formative
autoStress f
  | fStress f /= Penultimate = f  -- Explicitly set, don't override
  | otherwise = case fSlotIX f of
      Right _ -> f { fStress = Ultimate }
      Left _ -> f

sel1 :: (a, b, c) -> a
sel1 (a, _, _) = a
sel2 :: (a, b, c) -> b
sel2 (_, b, _) = b
sel3 :: (a, b, c) -> c
sel3 (_, _, c) = c

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
  TIO.putStrLn $ col bold "Ithkuil V4 Glosser" <> " " <> col dim ("(" <> T.pack (show (Map.size roots))
               <> " roots, " <> T.pack (show (Map.size affixes)) <> " affixes)")
  TIO.putStrLn $ col dim "Enter Ithkuil text (Ctrl-D to quit):"
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
          let stripped = T.strip line
          if T.null stripped
            then loop
            else do
              case T.uncons stripped of
                Just ('/', cmd) -> replCommand cmd
                _ -> glossLine roots affixes stripped
              TIO.putStrLn ""
              loop
    replCommand cmd
      | "root " `T.isPrefixOf` cmd = do
          let query = T.drop 5 cmd
              results = searchRoots query roots
          if null results
            then TIO.putStrLn $ col dim "No roots found for: " <> query
            else mapM_ (\(cr, entry) ->
              TIO.putStrLn $ col yellow ("-" <> cr <> "-") <> "  S1: " <> rootStem1 entry
                          <> "  S2: " <> rootStem2 entry) (take 10 results)
      | "affix " `T.isPrefixOf` cmd = do
          let query = T.drop 6 cmd
              results = searchAffixes query affixes
          if null results
            then TIO.putStrLn $ col dim "No affixes found for: " <> query
            else mapM_ (\(cs, entry) ->
              TIO.putStrLn $ col yellow ("-" <> cs <> "-") <> "  " <> affixAbbrev entry
                          <> "  (" <> affixDesc entry <> ")") (take 10 results)
      | "compose " `T.isPrefixOf` cmd = do
          let parts = T.words (T.drop 8 cmd)
          case parts of
            (root:opts) -> do
              let f0 = minimalFormative root
                  flags = map T.toUpper opts
                  f1 = applyComposeFlags flags f0
                  f2 = autoStress f1
                  word = composeFormative f2
              TIO.putStrLn $ "  " <> col bold word
              glossLine roots affixes word
            _ -> TIO.putStrLn "Usage: /compose <root> [S2] [DYN] [ABS] [IRG] ..."
      | "lookup " `T.isPrefixOf` cmd = do
          let query = T.drop 7 cmd
              results = lookupGrammar query
          case results of
            [] -> TIO.putStrLn $ col dim "Not found: " <> query
            _ -> mapM_ (\e -> TIO.putStrLn $ col bold (gAbbrev e) <> "  " <> gName e
                          <> "  [" <> gCategory e <> "]  form: " <> gForm e) results
      | "help" `T.isPrefixOf` cmd = do
          TIO.putStrLn $ col bold "REPL Commands:"
          TIO.putStrLn "  /root <keyword>           Search roots by meaning"
          TIO.putStrLn "  /affix <keyword>          Search affixes by keyword"
          TIO.putStrLn "  /compose <root> [opts]     Compose a formative"
          TIO.putStrLn "  /lookup <abbr>            Look up grammar abbreviation"
          TIO.putStrLn "  /help                     Show this help"
          TIO.putStrLn "  <ithkuil text>            Parse and gloss (default)"
      | otherwise = TIO.putStrLn $ col dim "Unknown command. Type /help for commands."

pipeMode :: Map.Map Text RootEntry -> Map.Map Text AffixEntry -> IO ()
pipeMode roots affixes = do
  contents <- TIO.getContents
  mapM_ (glossLine roots affixes . T.strip) (filter (not . T.null . T.strip) (T.lines contents))

glossLine :: Map.Map Text RootEntry -> Map.Map Text AffixEntry -> Text -> IO ()
glossLine roots affixes input = do
  let ws = T.words input
      -- Context-aware glossing: full glosses identify which words are foreign
      ctxFull = glossSentenceWords roots affixes input
      -- Build compact glosses, using foreign text pass-through from context
      compactGlosses = zipWith (\(_origW, fullG) w ->
        let clean = T.filter (\c -> c /= ',' && c /= '.' && c /= '!' && c /= '?' && c /= ':' && c /= ';') w
            parsed = parseWord clean
            isForeign = fullG == clean  -- context marked as foreign if gloss == raw word
        in if isForeign then clean
           else glossWordCompact roots affixes parsed
        ) ctxFull ws
  forM_ (zip ctxFull ws) $ \((_origW, fullG), w) ->
    let clean = T.filter (\c -> c /= ',' && c /= '.' && c /= '!' && c /= '?' && c /= ':' && c /= ';') w
    in if fullG == clean
      then TIO.putStrLn $ "  " <> col bold w <> "  " <> col dim "[Foreign text]"
      else glossOneWord roots affixes w
  -- Show interlinear summary with compact context-aware glosses
  when (length ws > 1) $ do
    let padTo n t = t <> T.replicate (max 0 (n - T.length t)) " "
        widths = zipWith (\w g -> max (T.length w) (T.length g) + 2) ws compactGlosses
    TIO.putStrLn ""
    TIO.putStr "  "
    mapM_ (\(w, n) -> TIO.putStr (col bold (padTo n w))) (zip ws widths)
    TIO.putStrLn ""
    TIO.putStr "  "
    mapM_ (\(g, n) -> TIO.putStr (col green (padTo n g))) (zip compactGlosses widths)
    TIO.putStrLn ""

glossOneWord :: Map.Map Text RootEntry -> Map.Map Text AffixEntry -> Text -> IO ()
glossOneWord roots affixes word = do
  let wtype = classifyWord word
      parsed = parseWord word
  TIO.putStrLn $ "  " <> col bold word <> "  " <> col dim ("[" <> T.pack (show wtype) <> "]")
  case parsed of
    PFormative pf -> showFormativeDetail roots affixes pf
    PConcatenated pfs -> do
      TIO.putStrLn $ "    Concatenation chain (" <> T.pack (show (length pfs)) <> " formatives):"
      mapM_ (\pf -> showFormativeDetail roots affixes pf) pfs
    PBias b -> TIO.putStrLn $ "    Bias: " <> T.pack (show b)
    PRegister r -> TIO.putStrLn $ "    Register: " <> T.pack (show r)
    PReferential refs mc vc ext -> do
      mapM_ (\ref -> showReferentialDetail ref mc vc) refs
      case ext of
        Just (wy, mc2, mRef2) -> do
          TIO.putStrLn $ "    Scope: " <> wy
          case mc2 of
            Just c2 -> TIO.putStrLn $ "    Case2: " <> T.pack (showCaseDetail c2)
            Nothing -> return ()
          case mRef2 of
            Just (PersonalRef r _) -> TIO.putStrLn $ "    Ref2: " <> referentLabel r
            Nothing -> return ()
        Nothing -> return ()
    PModular pairs fv _raw -> do
      mapM_ (\s8 -> TIO.putStrLn $ "    VnCn: " <> glossSlotVIII s8) pairs
      when (not (T.null fv)) $ TIO.putStrLn $ "    Final: " <> fv
    PAffixual cs deg _atype _ -> TIO.putStrLn $ "    Affix: " <> cs <> " degree " <> T.pack (show deg)
    PMultipleAffix (vx, cs) cz moreAfxs mVz -> do
      TIO.putStrLn $ "    First: -" <> cs <> "- " <> vx <> " (Cz=" <> cz <> ")"
      mapM_ (\(v, c) -> TIO.putStrLn $ "    Affix: -" <> c <> "- " <> v) moreAfxs
      case mVz of
        Just vz -> TIO.putStrLn $ "    Scope: " <> glossVz vz
        Nothing -> return ()
    PCombinationRef refs mc spec afxs mc2 -> do
      mapM_ (\(PersonalRef rr eff) ->
        TIO.putStrLn $ "    Referent: " <> referentLabel rr <> " /" <> T.pack (show eff)) refs
      case mc of
        Just c -> TIO.putStrLn $ "    Case: " <> T.pack (showCaseDetail c)
        Nothing -> return ()
      TIO.putStrLn $ "    Specification: " <> spec
      mapM_ (\(vx, cs) -> TIO.putStrLn $ "    Affix: " <> cs <> " " <> vx) afxs
      case mc2 of
        Just c2 -> TIO.putStrLn $ "    Case2: " <> T.pack (showCaseDetail c2)
        Nothing -> return ()
    PCarrier ct content -> TIO.putStrLn $ "    Carrier: " <> T.pack (show ct) <> " " <> content
    PMoodCaseScope ms -> TIO.putStrLn $ "    Mood/Case-Scope: " <> glossMoodOrScope ms
    PError msg _ -> TIO.putStrLn $ "    " <> col "\ESC[31m" ("ERROR: " <> msg)
    PUnparsed _ -> TIO.putStrLn $ "    " <> col dim "(unparsed)"
  TIO.putStrLn $ "    " <> col dim "GLOSS: " <> col green (glossWord roots affixes parsed)

showFormativeDetail :: Map.Map Text RootEntry -> Map.Map Text AffixEntry -> ParsedFormative -> IO ()
showFormativeDetail roots affixes pf = do
  let Root cr = pfRoot pf
      (stem, ver) = pfSlotII pf
      (func, spec, ctx) = pfSlotIV pf
      _conjs = pfConjuncts pf
  when (pfSentenceStarter pf) $
    TIO.putStrLn $ "    " <> col magenta "[Sentence starter]"
  -- Root or Cs affix
  case pfCsRootDegree pf of
    Just deg -> do
      TIO.putStrLn $ "    Cs-Root Affix: " <> col yellow ("-" <> cr <> "-") <> " degree " <> T.pack (show deg)
      TIO.putStrLn $ "    Version/Function: " <> col cyan (T.pack (show ver) <> "/" <> T.pack (show func))
      TIO.putStrLn $ "    Context: " <> col cyan (T.pack (show ctx))
    Nothing -> do
      let rootMeaning = case lookupRoot cr roots of
            Just entry -> selectStem stem entry
            Nothing -> "(not in lexicon)"
      TIO.putStrLn $ "    Root: " <> col yellow ("-" <> cr <> "-") <> " = " <> col green rootMeaning
      TIO.putStrLn $ "    Stem/Version: " <> col cyan (T.pack (show stem) <> "/" <> T.pack (show ver))
      TIO.putStrLn $ "    Function/Spec/Context: " <> col cyan (T.pack (show func) <> "/"
                   <> T.pack (show spec) <> "/" <> T.pack (show ctx))
  -- Ca
  case pfCaParsed pf of
    Just pc | pc /= ParsedCa UNI CSL M_ DEL NRM ->
      TIO.putStrLn $ "    Ca: " <> col cyan (showCaDetail pc)
    _ -> return ()
  -- Slot V affixes (CsVx order)
  mapM_ (\(cs, vx) -> do
    let compact = glossOneAffix affixes (vx, cs)
        degree = classifyDegree vx
        desc = case lookupAffix cs affixes of
          Just entry -> affixAbbrev entry <> " deg " <> T.pack (show degree)
          Nothing -> compact
    TIO.putStrLn $ "    SlotV: " <> col yellow ("-" <> cs <> "-") <> " " <> vx <> " = " <> desc
    ) (pfSlotV pf)
  -- Slot VII Affixes
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
    TIO.putStrLn $ "    Affix: " <> col yellow ("-" <> cs <> "-") <> " " <> vx <> " = " <> desc
    ) afxPairs
  -- Slot VIII: VnCn (from parse or extracted from Ca rest)
  -- Apply stress-based Mood vs CaseScope disambiguation
  let vnCn = fmap (disambiguateSlotVIII (pfStress pf)) $ case pfSlotVIII pf of
        Just s8 -> Just s8
        Nothing -> case extractVnCn (pfCa pf) of
          Just (vn, cn) -> parseOneVnCn vn cn
          Nothing -> Nothing
  case vnCn of
    Just s8 -> TIO.putStrLn $ "    VnCn: " <> col cyan (glossSlotVIII s8)
    Nothing -> return ()
  -- Slot IX: Case or Illocution+Validation
  case pfIllocVal pf of
    Just (ill, val) -> TIO.putStrLn $ "    Illocution/Validation: "
                     <> col cyan (T.pack (show ill) <> "/" <> T.pack (show val))
    Nothing -> case pfCase pf of
      Just c -> TIO.putStrLn $ "    Case: " <> col cyan (T.pack (showCaseDetail c))
      Nothing -> return ()
  -- Stress
  let stress = pfStress pf
  if stress /= Penultimate
    then TIO.putStrLn $ "    Stress: " <> col dim (T.pack (show stress))
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
