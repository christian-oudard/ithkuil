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
import Ithkuil.Compose (lookupGrammar, GrammarEntry(..), searchRoots, searchAffixes, dumpGrammarTable)

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
    ("--lookup":rest) -> handleLookup rest
    ("--root":rest) -> handleRootSearch rest
    ("--affix":rest) -> handleAffixSearch rest
    ("--grammar":rest) -> handleGrammarDump rest
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
      -- Context-aware glossing: carriers cause following words to be foreign text
      ctxPairs = glossSentenceWords roots affixes input
  mapM_ (glossOneWord roots affixes) ws
  -- Show interlinear summary with context-aware glosses
  when (length ws > 1) $ do
    let glosses = map snd ctxPairs
        origWs = map fst ctxPairs
        widths = zipWith (\w g -> max (T.length w) (T.length g) + 2) origWs glosses
        -- Pad based on visible text length, then apply color
        padTo n t = t <> T.replicate (max 0 (n - T.length t)) " "
    TIO.putStrLn ""
    TIO.putStr "  "
    mapM_ (\(w, n) -> TIO.putStr (col bold (padTo n w))) (zip origWs widths)
    TIO.putStrLn ""
    TIO.putStr "  "
    mapM_ (\(g, n) -> TIO.putStr (col green (padTo n g))) (zip glosses widths)
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
