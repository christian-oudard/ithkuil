{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Glossing Tool
-- Parses and glosses Ithkuil words and sentences
module Main where

import Control.Applicative ((<|>))
import Control.Monad (when, forM_)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(..), stdin, hIsTerminalDevice, hIsEOF)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import Ithkuil.Grammar
import Ithkuil.Parse (ParsedFormative(..), ParsedCa(..))
import Ithkuil.Referentials (PersonalRef(..), Referent(..), ReferentEffect(..), referentLabel)
import Ithkuil.WordType
import Ithkuil.Lexicon
import Ithkuil.Compose (lookupGrammar, searchGrammar, lookupForm, GrammarEntry(..), searchRootsRanked, searchAffixes, dumpGrammarTable, composeFormative, composeReferential, applyStress)
import Ithkuil.Render (renderSlotVIII)
import Ithkuil.Numbers (numberRoot)
import Ithkuil.Phonology (vowelForm)
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
    ("--form":rest) -> handleFormLookup rest
    ("--grammar":rest) -> handleGrammarDump rest
    ("--script":rest) -> handleScript rest
    ("--compose":rest) -> handleCompose rest
    ("--sentence":rest) -> handleSentence rest
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
handleLookup ws = do
  let query = T.pack (unwords ws)
      exact = lookupGrammar query
      results = if null exact then searchGrammar query else exact
  case results of
    [] -> TIO.putStrLn $ "No grammar entry found for: " <> query
    _ -> mapM_ (\e ->
      TIO.putStrLn $ gAbbrev e <> "  " <> gName e
                  <> "  [" <> gCategory e <> "]"
                  <> "  form: " <> gForm e) results

handleRootSearch :: [String] -> IO ()
handleRootSearch [] = TIO.putStrLn "Usage: ithkuil-gloss --root <keyword>"
handleRootSearch ws = do
  roots <- loadLexicon "data/roots.json"
  let query = T.pack (unwords ws)
      results = searchRootsRanked query roots
  if null results
    then TIO.putStrLn $ "No roots found matching: " <> query
    else mapM_ (\(_score, cr, entry) ->
      TIO.putStrLn $ "-" <> cr <> "-  S1: " <> rootStem1 entry
                  <> "  S2: " <> rootStem2 entry
                  <> "  S3: " <> rootStem3 entry
                  <> "  S0: " <> rootStem0 entry) (take 20 results)

handleFormLookup :: [String] -> IO ()
handleFormLookup [] = TIO.putStrLn "Usage: ithkuil-gloss --form <vowel|consonant>"
handleFormLookup ws = do
  let query = T.pack (unwords ws)
      results = lookupForm query
  case results of
    [] -> TIO.putStrLn $ "No grammar values use form: " <> query
    _ -> mapM_ (\e ->
      TIO.putStrLn $ gAbbrev e <> "  " <> gName e
                  <> "  [" <> gCategory e <> "]") results

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
  TIO.putStrLn "    opts: S0-S3 PRC CPT DYN STA BSC CTE CSV OBJ FRA +Cs/D ~Cs/D"
  TIO.putStrLn "          UNI DPX MSS G N A DEL PRX CSL ASO COA VAR NRM RPV"
  TIO.putStrLn "          THM ABS ERG DAT LOC ALL ABL PER ... (68 cases)"
  TIO.putStrLn "          OBS IRG DIR ADM DEC HOR POT CNJ VER (illocutions)"
  TIO.putStrLn "          RTR PRS HAB PRG IMM PCS REG ATP (aspects)"
  TIO.putStrLn "          +Cs/D = Slot VII affix, ~Cs/D = Slot V affix"
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
  let root = resolveRoot roots (T.pack rootStr)
      flags = map (resolveAffixFlag affixes . T.toUpper . T.pack) opts
      f0 = minimalFormative root
      f1 = applyComposeFlags flags f0
      f2 = autoStress f1
      word = composeFormative f2
  TIO.putStrLn $ col bold word
  glossLine roots affixes word

handleSentence :: [String] -> IO ()
handleSentence [] = TIO.putStrLn "Usage: ithkuil-gloss --sentence root:FLAG:FLAG @1m:ERG ..."
handleSentence specs = do
  roots <- loadLexicon "data/roots.json"
  affixes <- loadAffixLexicon "data/affixes.json"
  let words_ = map (composeOneWord roots affixes . T.pack) specs
      sentence = T.unwords words_
  TIO.putStrLn $ col bold sentence
  glossLine roots affixes sentence

-- | Compose one word spec: @referent:CASE, #VnCn modular adjunct, or root:FLAG:FLAG
-- Root can be consonant cluster (e.g. "rţt") or English keyword (e.g. "study")
-- Affix flags like +NEG/4 are resolved via the affix lexicon.
composeOneWord :: Map.Map Text RootEntry -> Map.Map Text AffixEntry -> Text -> Text
composeOneWord roots affixes s = case T.splitOn ":" s of
  (w:opts)
    | Just ref <- T.stripPrefix "@" w
    , let flags = map T.toUpper opts
    -> composeRefWord (T.toUpper ref) flags
  (w:opts)
    | Just _ <- T.stripPrefix "#" w
    -> composeModularWord (map T.toUpper (w:opts))
  (w:opts)
    | Just numStr <- T.stripPrefix "%" w
    , [(n, "")] <- reads (T.unpack numStr) :: [(Int, String)]
    , n >= 0, n < 100
    -> let cr = numberRoot n
           f0 = minimalFormative cr
           flags = map (resolveAffixFlag affixes . T.toUpper) opts
           f1 = applyComposeFlags flags f0
           f2 = autoStress f1
       in composeFormative f2
  (root:opts) ->
    let resolved = resolveRoot roots root
        f0 = minimalFormative resolved
        flags = map (resolveAffixFlag affixes . T.toUpper) opts
        f1 = applyComposeFlags flags f0
        f2 = autoStress f1
    in composeFormative f2
  [] -> "?"

-- | Compose a modular adjunct word from flags
-- Syntax: #VnCategory.Mood (e.g. #RTR.SUB, #PRG.FAC, #PUN.HYP)
-- Ultimate stress marks the final vowel as Vh scope (formative scope = "a")
-- Penultimate stress means the final vowel is another VnCn with implicit Cn="h"
composeModularWord :: [Text] -> Text
composeModularWord flags =
  let pairs = mapMaybe parseModularFlag flags
      rendered = T.concat $ map (renderSlotVIII . Just) pairs
  in if T.null rendered then "?"
     else applyStress Ultimate (rendered <> "a")
  where
    parseModularFlag :: Text -> Maybe SlotVIII
    parseModularFlag t =
      let stripped = maybe t id (T.stripPrefix "#" t)
      in case T.splitOn "." stripped of
        [vn, ms] -> buildVnCn vn ms
        [vn] -> buildVnCn vn "FAC"  -- default mood
        _ -> Nothing
    buildVnCn :: Text -> Text -> Maybe SlotVIII
    buildVnCn vnName msName =
      let ms = parseMoodOrScope msName
          -- Try each Vn category
      in case ms of
        Nothing -> Nothing
        Just mood -> tryAspect vnName mood
                 <|> tryValence vnName mood
                 <|> tryPhase vnName mood
                 <|> tryEffect vnName mood
                 <|> tryLevel vnName mood
    parseMoodOrScope :: Text -> Maybe MoodOrScope
    parseMoodOrScope "FAC" = Just (MoodVal FAC)
    parseMoodOrScope "SUB" = Just (MoodVal SUB)
    parseMoodOrScope "ASM" = Just (MoodVal ASM)
    parseMoodOrScope "SPC" = Just (MoodVal SPC)
    parseMoodOrScope "COU" = Just (MoodVal COU)
    parseMoodOrScope "HYP" = Just (MoodVal HYP)
    parseMoodOrScope "CCN" = Just (CaseScope CCN)
    parseMoodOrScope "CCA" = Just (CaseScope CCA)
    parseMoodOrScope "CCS" = Just (CaseScope CCS)
    parseMoodOrScope "CCQ" = Just (CaseScope CCQ)
    parseMoodOrScope "CCP" = Just (CaseScope CCP)
    parseMoodOrScope "CCV" = Just (CaseScope CCV)
    parseMoodOrScope _ = Nothing
    tryAspect n ms = lookup n aspectNames >>= \asp -> Just (VnCnAspect asp ms)
    tryValence n ms = lookup n valenceNames >>= \v -> Just (VnCnValence v ms)
    tryPhase n ms = lookup n phaseNames >>= \p -> Just (VnCnPhase p ms)
    tryEffect n ms = lookup n effectNames >>= \e -> Just (VnCnEffect e ms)
    tryLevel n ms = lookup n levelNames >>= \l -> Just (VnCnLevel l False ms)
    aspectNames = [(T.pack (show a), a) | a <- allOf]
    valenceNames = [(T.pack (show v), v) | v <- allOf]
    phaseNames = [(T.pack (show p), p) | p <- allOf]
    effectNames = [(T.pack (show e), e) | e <- allOf]
    levelNames = [(T.pack (show l), l) | l <- allOf]

-- | Resolve affix abbreviation in a flag: +NEG/4 → +r/4
resolveAffixFlag :: Map.Map Text AffixEntry -> Text -> Text
resolveAffixFlag affixes flag
  | Just rest <- T.stripPrefix "+" flag = "+" <> resolveAffixCs affixes rest
  | Just rest <- T.stripPrefix "~" flag = "~" <> resolveAffixCs affixes rest
  | otherwise = flag

-- | Resolve affix Cs: if "NEG/4" and NEG is a known abbreviation, replace with "r/4"
resolveAffixCs :: Map.Map Text AffixEntry -> Text -> Text
resolveAffixCs affixes t = case T.splitOn "/" t of
  [cs, deg] ->
    let csLower = T.toLower cs
        -- Check if cs is already a consonant form
        isConsonant = Map.member csLower affixes
        -- Try as abbreviation
        abbrevMatch = [(affixCs e) | e <- Map.elems affixes, T.toUpper (affixAbbrev e) == cs]
    in if isConsonant then t
       else case abbrevMatch of
         (realCs:_) -> realCs <> "/" <> deg
         [] -> t
  _ -> t

-- | Resolve a root: if it contains vowels, search the lexicon by keyword
-- and use the first match's consonant form. Uses ranked scoring from Compose.
resolveRoot :: Map.Map Text RootEntry -> Text -> Text
resolveRoot roots t
  | T.any isVowelChar t =
    case searchRootsRanked t roots of
      ((_,cr,_):_) -> cr
      [] -> t
  | otherwise = stripDashes t
  where
    isVowelChar c = c `elem` ("aäeëiïoöuüáéíóú" :: String)
    stripDashes = T.dropWhile (== '-') . T.dropWhileEnd (== '-')

-- | Compose a referential word: @REF:CASE → C1+Vc
composeRefWord :: Text -> [Text] -> Text
composeRefWord ref flags =
  let refM = parseReferent ref
      caseM = findCase flags
  in case refM of
    Just pr -> composeReferential pr (maybe (Transrelative THM) id caseM)
    Nothing -> "?" <> ref

-- | Parse referent abbreviation to PersonalRef
parseReferent :: Text -> Maybe PersonalRef
parseReferent "1M"  = Just (PersonalRef R1m NEU)
parseReferent "2M"  = Just (PersonalRef R2m NEU)
parseReferent "2P"  = Just (PersonalRef R2p NEU)
parseReferent "MA"  = Just (PersonalRef Rma NEU)
parseReferent "PA"  = Just (PersonalRef Rpa NEU)
parseReferent "MI"  = Just (PersonalRef Rmi NEU)
parseReferent "PI"  = Just (PersonalRef Rpi NEU)
parseReferent "MX"  = Just (PersonalRef Rmx NEU)
parseReferent "RDP" = Just (PersonalRef Rrdp NEU)
parseReferent "OBV" = Just (PersonalRef Robv NEU)
parseReferent "PVS" = Just (PersonalRef Rpvs NEU)
-- With effect: 1M.BEN, MA.DET, etc.
parseReferent t = case T.splitOn "." t of
  [r, "BEN"] -> fmap (\(PersonalRef ref _) -> PersonalRef ref BEN) (parseReferent r)
  [r, "DET"] -> fmap (\(PersonalRef ref _) -> PersonalRef ref DET) (parseReferent r)
  _ -> Nothing

-- | Find a case flag in the flag list
findCase :: [Text] -> Maybe Case
findCase [] = Nothing
findCase (f:fs) = case parseCaseFlag f of
  Just c -> Just c
  Nothing -> findCase fs

-- | Parse a case abbreviation to a Case value
parseCaseFlag :: Text -> Maybe Case
parseCaseFlag t =
  let f0 = minimalFormative "x"
      f1 = applyOneFlag t f0
  in case fSlotIX f1 of
    Left c | fSlotIX f1 /= fSlotIX f0 -> Just c
    _ -> Nothing

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
applyOneFlag "ASS" f = f { fSlotIX = Left (SpatioTemporal2 ASS) }
applyOneFlag "PER" f = f { fSlotIX = Left (SpatioTemporal2 PER) }
applyOneFlag "PRO" f = f { fSlotIX = Left (SpatioTemporal2 PRO) }
applyOneFlag "PCV" f = f { fSlotIX = Left (SpatioTemporal2 PCV) }
applyOneFlag "PCR" f = f { fSlotIX = Left (SpatioTemporal2 PCR) }
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
-- Relational Cases
applyOneFlag "PRN" f = f { fSlotIX = Left (Relational PRN) }
applyOneFlag "DSP" f = f { fSlotIX = Left (Relational DSP) }
applyOneFlag "COR" f = f { fSlotIX = Left (Relational COR) }
applyOneFlag "CPS" f = f { fSlotIX = Left (Relational CPS) }
applyOneFlag "COM" f = f { fSlotIX = Left (Relational COM) }
applyOneFlag "UTL" f = f { fSlotIX = Left (Relational UTL) }
applyOneFlag "PRD" f = f { fSlotIX = Left (Relational PRD) }
applyOneFlag "RLT" f = f { fSlotIX = Left (Relational RLT) }
-- Affinitive Cases
applyOneFlag "ACT" f = f { fSlotIX = Left (Affinitive ACT) }
applyOneFlag "ASI" f = f { fSlotIX = Left (Affinitive ASI) }
applyOneFlag "ESS" f = f { fSlotIX = Left (Affinitive ESS) }
applyOneFlag "TRM" f = f { fSlotIX = Left (Affinitive TRM) }
applyOneFlag "SEL" f = f { fSlotIX = Left (Affinitive SEL) }
applyOneFlag "CFM" f = f { fSlotIX = Left (Affinitive CFM) }
applyOneFlag "DEP" f = f { fSlotIX = Left (Affinitive DEP) }
applyOneFlag "VOC" f = f { fSlotIX = Left (Affinitive VOC) }
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
-- Aspect (Slot VIII, Pattern 2) — all 36 aspects
applyOneFlag "RTR" f = setAspect RTR f
applyOneFlag "PRS" f = setAspect PRS f
applyOneFlag "HAB" f = setAspect HAB f
applyOneFlag "PRG" f = setAspect PRG f
applyOneFlag "IMM" f = setAspect IMM f
applyOneFlag "PCS" f = setAspect PCS f
applyOneFlag "REG" f = setAspect REG f
applyOneFlag "SMM" f = setAspect SMM f
applyOneFlag "ATP" f = setAspect ATP f
applyOneFlag "RSM" f = setAspect RSM f
applyOneFlag "CSS" f = setAspect CSS f
applyOneFlag "PAU" f = setAspect PAU f
applyOneFlag "RGR" f = setAspect RGR f
applyOneFlag "PCL" f = setAspect PCL f
applyOneFlag "CNT" f = setAspect CNT f
applyOneFlag "ICS" f = setAspect ICS f
applyOneFlag "EXP" f = setAspect EXP f
applyOneFlag "IRP" f = setAspect IRP f
applyOneFlag "PMP" f = setAspect PMP f
applyOneFlag "CLM" f = setAspect CLM f
applyOneFlag "DLT" f = setAspect DLT f
applyOneFlag "TMP" f = setAspect TMP f
applyOneFlag "XPD" f = setAspect XPD f
applyOneFlag "LIM" f = setAspect LIM f
applyOneFlag "EPD" f = setAspect EPD f
applyOneFlag "PTC" f = setAspect PTC f
applyOneFlag "PPR" f = setAspect PPR f
applyOneFlag "DCL" f = setAspect DCL f
applyOneFlag "CCL" f = setAspect CCL f
applyOneFlag "CUL" f = setAspect CUL f
applyOneFlag "IMD" f = setAspect IMD f
applyOneFlag "TRD" f = setAspect TRD f
applyOneFlag "TNS" f = setAspect TNS f
applyOneFlag "ITC" f = setAspect ITC f
applyOneFlag "MTV" f = setAspect MTV f
applyOneFlag "SQN" f = setAspect SQN f
-- Valence (Slot VIII, Pattern 1)
applyOneFlag "MNO" f = setValence MNO f
applyOneFlag "PRL" f = setValence PRL f
applyOneFlag "CRO" f = setValence CRO f
applyOneFlag "RCP" f = setValence RCP f
applyOneFlag "CPL" f = setValence CPL f
applyOneFlag "DUP" f = setValence DUP f
applyOneFlag "DEM" f = setValence DEM f
applyOneFlag "CNG" f = setValence CNG f
applyOneFlag "PTI" f = setValence PTI f
-- Phase (Slot VIII, Pattern 1 Series 2)
applyOneFlag "PUN" f = setPhase PUN f
applyOneFlag "ITR" f = setPhase ITR f
applyOneFlag "REP" f = setPhase REP f
applyOneFlag "ITM" f = setPhase ITM f
applyOneFlag "RCT" f = setPhase RCT f
applyOneFlag "FRE" f = setPhase FRE f
applyOneFlag "FRG" f = setPhase FRG f
applyOneFlag "VAC" f = setPhase VAC f
applyOneFlag "FLC" f = setPhase FLC f
-- Effect (Slot VIII, Pattern 1 Series 3)
applyOneFlag "BEN1" f = setEffect BEN1 f
applyOneFlag "BEN2" f = setEffect BEN2 f
applyOneFlag "BEN3" f = setEffect BEN3 f
applyOneFlag "BSLF" f = setEffect BSLF f
applyOneFlag "UNKN" f = setEffect UNK f
applyOneFlag "DSLF" f = setEffect DSLF f
applyOneFlag "DET3" f = setEffect DET3 f
applyOneFlag "DET2" f = setEffect DET2 f
applyOneFlag "DET1" f = setEffect DET1 f
-- Level (Slot VIII, Pattern 1 Series 4)
applyOneFlag "MIN" f = setLevel MIN False f
applyOneFlag "SBE" f = setLevel SBE False f
applyOneFlag "IFR" f = setLevel IFR False f
applyOneFlag "DFT" f = setLevel DFT False f
applyOneFlag "EQU" f = setLevel EQU False f
applyOneFlag "SUR" f = setLevel SUR False f
applyOneFlag "SPL" f = setLevel SPL False f
applyOneFlag "SPQ" f = setLevel SPQ False f
applyOneFlag "MAX" f = setLevel MAX False f
-- Mood (Cn modifier)
applyOneFlag "FAC" f = setMood (MoodVal FAC) f
applyOneFlag "SUB" f = setMood (MoodVal SUB) f
applyOneFlag "ASM" f = setMood (MoodVal ASM) f
applyOneFlag "SPC" f = setMood (MoodVal SPC) f
applyOneFlag "COU" f = setMood (MoodVal COU) f
applyOneFlag "HYP" f = setMood (MoodVal HYP) f
-- Case-Scope (Cn modifier for nouns)
applyOneFlag "CCN" f = setMood (CaseScope CCN) f
applyOneFlag "CCA" f = setMood (CaseScope CCA) f
applyOneFlag "CCS" f = setMood (CaseScope CCS) f
applyOneFlag "CCQ" f = setMood (CaseScope CCQ) f
applyOneFlag "CCP" f = setMood (CaseScope CCP) f
applyOneFlag "CCV" f = setMood (CaseScope CCV) f
-- Framed relation (antepenultimate stress)
applyOneFlag "FRA" f = f { fStress = Antepenultimate }
-- Affix: +Cs/D for Slot VII (Ca-scoped), ~Cs/D for Slot V (stem-scoped)
applyOneFlag flag f
  | Just rest <- T.stripPrefix "+" flag = case parseAffixFlag rest of
      Just afx -> f { fSlotVII = fSlotVII f ++ [afx] }
      Nothing -> f
  | Just rest <- T.stripPrefix "~" flag = case parseAffixFlag rest of
      Just afx -> f { fSlotV = fSlotV f ++ [afx] }
      Nothing -> f
  | otherwise = f  -- Ignore unknown flags

-- | Parse affix flag like "fm/2" or "fm/2₂" → Affix with vowel form
parseAffixFlag :: Text -> Maybe Affix
parseAffixFlag t = case T.splitOn "/" t of
  [cs, degStr] ->
    let (degDigit, typeStr) = T.span (\c -> c >= '0' && c <= '9') (T.toLower degStr)
        deg = case reads (T.unpack degDigit) :: [(Int, String)] of
          [(d, "")] | d >= 0 && d <= 9 -> d
          _ -> -1
        atype = case typeStr of
          "₂" -> 2; "₃" -> 3; _ -> 1
        vx = if deg == 0 then case atype of
               1 -> "ae"; 2 -> "ea"; 3 -> "üo"; _ -> "ae"
             else vowelForm atype deg
    in if deg >= 0 then Just (Affix vx (T.toLower cs) (toAffixType atype))
       else Nothing
  _ -> Nothing
  where
    toAffixType 1 = Type1Affix; toAffixType 2 = Type2Affix
    toAffixType 3 = Type3Affix; toAffixType _ = Type1Affix

-- | Set validation on existing illocution, or default to ASR
setVal :: Validation -> Formative -> Formative
setVal v f = case fSlotIX f of
  Right (IllocVal ill _) -> f { fSlotIX = Right (IllocVal ill v) }
  _ -> f { fSlotIX = Right (IllocVal ASR v) }

-- | Set aspect, preserving existing mood/scope
setAspect :: Aspect -> Formative -> Formative
setAspect asp f = f { fSlotVIII = Just (VnCnAspect asp (getMoodOrScope f)) }

-- | Set mood or case-scope, preserving existing VnCn pattern
setMood :: MoodOrScope -> Formative -> Formative
setMood ms f = case fSlotVIII f of
  Just (VnCnAspect asp _)  -> f { fSlotVIII = Just (VnCnAspect asp ms) }
  Just (VnCnValence v _)   -> f { fSlotVIII = Just (VnCnValence v ms) }
  Just (VnCnPhase p _)     -> f { fSlotVIII = Just (VnCnPhase p ms) }
  Just (VnCnEffect e _)    -> f { fSlotVIII = Just (VnCnEffect e ms) }
  Just (VnCnLevel l b _)   -> f { fSlotVIII = Just (VnCnLevel l b ms) }
  _ -> f { fSlotVIII = Just (VnCnValence MNO ms) }

-- | Set valence, preserving existing mood/scope
setValence :: Valence -> Formative -> Formative
setValence v f = f { fSlotVIII = Just (VnCnValence v (getMoodOrScope f)) }

-- | Set phase, preserving existing mood/scope
setPhase :: Phase -> Formative -> Formative
setPhase p f = f { fSlotVIII = Just (VnCnPhase p (getMoodOrScope f)) }

-- | Set effect, preserving existing mood/scope
setEffect :: Effect -> Formative -> Formative
setEffect e f = f { fSlotVIII = Just (VnCnEffect e (getMoodOrScope f)) }

-- | Set level, preserving existing mood/scope
setLevel :: Level -> Bool -> Formative -> Formative
setLevel l abs_ f = f { fSlotVIII = Just (VnCnLevel l abs_ (getMoodOrScope f)) }

-- | Extract mood/scope from current SlotVIII, defaulting to FAC
getMoodOrScope :: Formative -> MoodOrScope
getMoodOrScope f = case fSlotVIII f of
  Just (VnCnAspect _ ms)  -> ms
  Just (VnCnValence _ ms) -> ms
  Just (VnCnPhase _ ms)   -> ms
  Just (VnCnEffect _ ms)  -> ms
  Just (VnCnLevel _ _ ms) -> ms
  _ -> MoodVal FAC

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

-- | Auto-determine stress from Slot IX and VnCn mood, unless explicitly set (e.g. FRA)
autoStress :: Formative -> Formative
autoStress f
  | fStress f /= Penultimate = f  -- Explicitly set, don't override
  | otherwise = case fSlotIX f of
      Right _ -> f { fStress = Ultimate }
      Left _ -> case fSlotVIII f of
        -- Mood (non-FAC) implies verbal → ultimate stress
        Just s8 | hasMood s8 -> f { fStress = Ultimate }
        _ -> f
  where
    hasMood s8 = case s8 of
      VnCnAspect _ (MoodVal m)  -> m /= FAC
      VnCnValence _ (MoodVal m) -> m /= FAC
      VnCnPhase _ (MoodVal m)   -> m /= FAC
      VnCnEffect _ (MoodVal m)  -> m /= FAC
      VnCnLevel _ _ (MoodVal m) -> m /= FAC
      _ -> False

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
              results = searchRootsRanked query roots
          if null results
            then TIO.putStrLn $ col dim "No roots found for: " <> query
            else mapM_ (\(_s, cr, entry) ->
              if length results <= 3
                then TIO.putStrLn $ col yellow ("-" <> cr <> "-")
                  <> "\n    S0: " <> rootStem0 entry
                  <> "\n    S1: " <> rootStem1 entry
                  <> "\n    S2: " <> rootStem2 entry
                  <> "\n    S3: " <> rootStem3 entry
                else TIO.putStrLn $ col yellow ("-" <> cr <> "-") <> "  " <> rootStem0 entry
              ) (take 10 results)
      | "affix " `T.isPrefixOf` cmd = do
          let query = T.drop 6 cmd
              results = searchAffixes query affixes
          if null results
            then TIO.putStrLn $ col dim "No affixes found for: " <> query
            else mapM_ (\(cs, entry) -> do
              TIO.putStrLn $ col yellow ("-" <> cs <> "-") <> "  " <> affixAbbrev entry
                          <> "  (" <> affixDesc entry <> ")"
              when (length results <= 3) $
                forM_ (zip [1::Int ..] (affixDegrees entry)) $ \(d, desc) ->
                  TIO.putStrLn $ "    " <> T.pack (show d) <> ": " <> desc
              ) (take 10 results)
      | "compose " `T.isPrefixOf` cmd = do
          let parts = T.words (T.drop 8 cmd)
          case parts of
            (root:opts) -> do
              let resolved = resolveRoot roots root
                  f0 = minimalFormative resolved
                  flags = map (resolveAffixFlag affixes . T.toUpper) opts
                  f1 = applyComposeFlags flags f0
                  f2 = autoStress f1
                  word = composeFormative f2
              TIO.putStrLn $ "  " <> col bold word
              glossLine roots affixes word
            _ -> TIO.putStrLn "Usage: /compose <root> [S2] [DYN] [ABS] [IRG] ..."
      | "sentence " `T.isPrefixOf` cmd = do
          let specs = T.words (T.drop 9 cmd)
              words_ = map (composeOneWord roots affixes) specs
              sentence = T.unwords words_
          TIO.putStrLn $ "  " <> col bold sentence
          glossLine roots affixes sentence
      | "lookup " `T.isPrefixOf` cmd = do
          let query = T.drop 7 cmd
              exact = lookupGrammar query
              results = if null exact then searchGrammar query else exact
          case results of
            [] -> TIO.putStrLn $ col dim "Not found: " <> query
            _ -> mapM_ (\e -> TIO.putStrLn $ col bold (gAbbrev e) <> "  " <> gName e
                          <> "  [" <> gCategory e <> "]  form: " <> gForm e) results
      | "form " `T.isPrefixOf` cmd = do
          let query = T.drop 5 cmd
              results = lookupForm query
          case results of
            [] -> TIO.putStrLn $ col dim "No grammar values use form: " <> query
            _ -> mapM_ (\e -> TIO.putStrLn $ col bold (gAbbrev e) <> "  " <> gName e
                          <> "  [" <> gCategory e <> "]") results
      | "help" `T.isPrefixOf` cmd = do
          TIO.putStrLn $ col bold "REPL Commands:"
          TIO.putStrLn "  /root <keyword|Cr>        Search roots (English or -Cr- form)"
          TIO.putStrLn "  /affix <keyword|Cs>       Search affixes by keyword or Cs form"
          TIO.putStrLn "  /lookup <abbr|name>       Look up grammar (abbr, name, or category)"
          TIO.putStrLn "  /form <vowel|consonant>   Reverse lookup: form -> grammar values"
          TIO.putStrLn "  /compose <root> [opts]     Compose a formative"
          TIO.putStrLn "  /sentence r:F:F r2:F ...  Compose multi-word sentence"
          TIO.putStrLn "  /help                     Show this help"
          TIO.putStrLn "  <ithkuil text>            Parse and gloss (default)"
      | otherwise = TIO.putStrLn $ col dim "Unknown command. Type /help for commands."

pipeMode :: Map.Map Text RootEntry -> Map.Map Text AffixEntry -> IO ()
pipeMode roots affixes = do
  contents <- TIO.getContents
  let processLine line =
        let stripped = T.strip line
        in case T.uncons stripped of
          Just ('/', cmd) -> pipeCommand roots affixes cmd
          _ -> glossLine roots affixes stripped
  mapM_ processLine (filter (not . T.null . T.strip) (T.lines contents))

pipeCommand :: Map.Map Text RootEntry -> Map.Map Text AffixEntry -> Text -> IO ()
pipeCommand roots affixes cmd
  | "root " `T.isPrefixOf` cmd = do
      let query = T.drop 5 cmd
          results = searchRootsRanked query roots
      if null results
        then TIO.putStrLn $ "No roots found for: " <> query
        else mapM_ (\(_s, cr, entry) ->
          if length results <= 3
            then TIO.putStrLn $ "-" <> cr <> "-"
              <> "\n  S0: " <> rootStem0 entry
              <> "\n  S1: " <> rootStem1 entry
              <> "\n  S2: " <> rootStem2 entry
              <> "\n  S3: " <> rootStem3 entry
            else TIO.putStrLn $ "-" <> cr <> "-  " <> rootStem0 entry
          ) (take 10 results)
  | "affix " `T.isPrefixOf` cmd = do
      let query = T.drop 6 cmd
          results = searchAffixes query affixes
      if null results
        then TIO.putStrLn $ "No affixes found for: " <> query
        else mapM_ (\(cs, entry) -> do
          TIO.putStrLn $ "-" <> cs <> "-  " <> affixAbbrev entry
                      <> "  (" <> affixDesc entry <> ")"
          when (length results <= 3) $
            forM_ (zip [1::Int ..] (affixDegrees entry)) $ \(d, desc) ->
              TIO.putStrLn $ "  " <> T.pack (show d) <> ": " <> desc
          ) (take 10 results)
  | "lookup " `T.isPrefixOf` cmd = do
      let query = T.drop 7 cmd
          exact = lookupGrammar query
          results = if null exact then searchGrammar query else exact
      case results of
        [] -> TIO.putStrLn $ "Not found: " <> query
        _ -> mapM_ (\e -> TIO.putStrLn $ gAbbrev e <> "  " <> gName e
                      <> "  [" <> gCategory e <> "]  form: " <> gForm e) results
  | "form " `T.isPrefixOf` cmd = do
      let query = T.drop 5 cmd
          results = lookupForm query
      case results of
        [] -> TIO.putStrLn $ "No grammar values use form: " <> query
        _ -> mapM_ (\e -> TIO.putStrLn $ gAbbrev e <> "  " <> gName e
                      <> "  [" <> gCategory e <> "]") results
  | "sentence " `T.isPrefixOf` cmd = do
      let specs = T.words (T.drop 9 cmd)
          words_ = map (composeOneWord roots affixes) specs
          sentence = T.unwords words_
      TIO.putStrLn sentence
      glossLine roots affixes sentence
  | "compose " `T.isPrefixOf` cmd = do
      let parts = T.words (T.drop 8 cmd)
      case parts of
        (root:opts) -> do
          let resolved = resolveRoot roots root
              f0 = minimalFormative resolved
              flags = map (resolveAffixFlag affixes . T.toUpper) opts
              f1 = applyComposeFlags flags f0
              f2 = autoStress f1
              word = composeFormative f2
          TIO.putStrLn word
          glossLine roots affixes word
        _ -> TIO.putStrLn "Usage: /compose <root> [S2] [DYN] [ABS] [IRG] ..."
  | otherwise = TIO.putStrLn $ "Unknown command: /" <> cmd

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
