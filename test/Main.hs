{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.Maybe (isJust, isNothing, fromJust)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Ithkuil.Phonology
import Ithkuil.Grammar
import Ithkuil.Parse
import Ithkuil.Render
import Ithkuil.FullParse
import Ithkuil.Allomorph (constructCa, parseCaSlot, renderCa)
import Ithkuil.Gloss

main :: IO ()
main = hspec $ do
  describe "Phonology" $ do
    it "has 9 vowels" $ do
      length vowels `shouldBe` 9

    it "has 31 consonants" $ do
      length consonants `shouldBe` 31

    it "vowel form table has 4 series" $ do
      length vowelFormTable `shouldBe` 4

    it "each series has 9 forms" $ do
      all (\row -> length row == 9) vowelFormTable `shouldBe` True

    it "vowelForm 1 1 = a" $
      vowelForm 1 1 `shouldBe` "a"

    it "vowelForm 2 1 = ai" $
      vowelForm 2 1 `shouldBe` "ai"

    it "vowelForm 3 1 = ia" $
      vowelForm 3 1 `shouldBe` "ia"

  describe "Grammar" $ do
    it "slotIIToVv encodes stem 1 processual as 'a'" $ do
      slotIIToVv (S1, PRC) `shouldBe` "a"

    it "slotIIToVv encodes stem 1 completive as 'ä'" $ do
      slotIIToVv (S1, CPT) `shouldBe` "ä"

    it "can create minimal formative" $ do
      let f = minimalFormative "ml"
      fSlotIII f `shouldBe` Root "ml"
      fSlotII f `shouldBe` defaultSlotII
      fSlotVI f `shouldBe` defaultSlotVI

  describe "Configuration" $ do
    it "has 19 non-uniplex values" $ do
      length (filter (/= UNI) allOf) `shouldBe` 18

  describe "Slot II Parsing" $ do
    it "parses all 8 Vv forms" $ do
      parseSlotII "a" `shouldBe` Just (S1, PRC)
      parseSlotII "ä" `shouldBe` Just (S1, CPT)
      parseSlotII "e" `shouldBe` Just (S2, PRC)
      parseSlotII "i" `shouldBe` Just (S2, CPT)
      parseSlotII "u" `shouldBe` Just (S3, PRC)
      parseSlotII "ü" `shouldBe` Just (S3, CPT)
      parseSlotII "o" `shouldBe` Just (S0, PRC)
      parseSlotII "ö" `shouldBe` Just (S0, CPT)

    it "rejects invalid Vv" $
      parseSlotII "x" `shouldBe` Nothing

  describe "Slot IV Parsing" $ do
    it "parses EXS context (series 1)" $ do
      parseSlotIV "a" `shouldBe` Just (STA, BSC, EXS)
      parseSlotIV "ä" `shouldBe` Just (STA, CTE, EXS)
      parseSlotIV "e" `shouldBe` Just (STA, CSV, EXS)
      parseSlotIV "i" `shouldBe` Just (STA, OBJ, EXS)
      parseSlotIV "u" `shouldBe` Just (DYN, BSC, EXS)

    it "parses FNC context (series 2)" $ do
      parseSlotIV "ai" `shouldBe` Just (STA, BSC, FNC)
      parseSlotIV "eu" `shouldBe` Just (STA, OBJ, FNC)
      parseSlotIV "ui" `shouldBe` Just (DYN, BSC, FNC)

    it "parses RPS context (series 3)" $ do
      parseSlotIV "ia" `shouldBe` Just (STA, BSC, RPS)
      parseSlotIV "ië" `shouldBe` Just (STA, OBJ, RPS)
      parseSlotIV "ua" `shouldBe` Just (DYN, BSC, RPS)

    it "parses AMG context (series 4)" $ do
      parseSlotIV "ao" `shouldBe` Just (STA, BSC, AMG)
      parseSlotIV "eo" `shouldBe` Just (STA, OBJ, AMG)
      parseSlotIV "oa" `shouldBe` Just (DYN, BSC, AMG)

  describe "Case Parsing" $ do
    it "parses all 9 transrelative cases" $ do
      parseCase "a" `shouldBe` Just (Transrelative THM)
      parseCase "ä" `shouldBe` Just (Transrelative INS)
      parseCase "e" `shouldBe` Just (Transrelative ABS)
      parseCase "i" `shouldBe` Just (Transrelative AFF)
      parseCase "o" `shouldBe` Just (Transrelative ERG)
      parseCase "u" `shouldBe` Just (Transrelative IND)

    it "parses appositive cases" $ do
      parseCase "ai" `shouldBe` Just (Appositive POS)
      parseCase "ei" `shouldBe` Just (Appositive GEN)

    it "parses associative cases (series 3 vowels)" $ do
      parseCase "ia" `shouldBe` Just (Associative APL)
      parseCase "ëu" `shouldBe` Just (Associative CRS)

    it "parses adverbial cases (series 4 vowels)" $ do
      parseCase "ao" `shouldBe` Just (Adverbial FUN)
      parseCase "oa" `shouldBe` Just (Adverbial SIT)

    it "parses spatio-temporal cases (glottal stop)" $ do
      parseCase "a'a" `shouldBe` Just (SpatioTemporal1 LOC)
      parseCase "i'i" `shouldBe` Just (SpatioTemporal2 CNR)

  describe "Ca Complex" $ do
    it "default Ca (UNI/CSL/M_/DEL/NRM) = l" $ do
      constructCa defaultSlotVI `shouldBe` "l"

    it "parseCaSlot reverses constructCa for default" $ do
      parseCaSlot "l" `shouldBe` Just defaultSlotVI

    it "DSS configuration = rt" $ do
      let ca = (DSS, CSL, M_, DEL, NRM)
      constructCa ca `shouldBe` "rt"

    it "G_ perspective (standalone) = r" $ do
      let ca = (UNI, CSL, G_, DEL, NRM)
      constructCa ca `shouldBe` "r"

    it "N_ perspective (standalone) = v" $ do
      let ca = (UNI, CSL, N_, DEL, NRM)
      constructCa ca `shouldBe` "v"

    it "A_ perspective (standalone) = z" $ do
      let ca = (UNI, CSL, A_, DEL, NRM)
      constructCa ca `shouldBe` "z"

    it "ASO affiliation (standalone) = d + l -> dl" $ do
      let ca = (UNI, CSL, M_, DEL, NRM)  -- This is default, ASO is different
      constructCa (UNI, ASO, M_, DEL, NRM) `shouldSatisfy` (not . T.null)

    it "RPV essence (standalone) = ř" $ do
      let ca = (UNI, CSL, M_, DEL, RPV)
      constructCa ca `shouldBe` "ř"

    it "Ca round-trips for common patterns" $ do
      let testRoundTrip s = parseCaSlot (constructCa s) `shouldBe` Just s
      testRoundTrip defaultSlotVI
      testRoundTrip (DSS, CSL, M_, DEL, NRM)
      testRoundTrip (UNI, CSL, G_, DEL, NRM)
      testRoundTrip (UNI, CSL, A_, DEL, NRM)
      testRoundTrip (UNI, CSL, M_, DEL, RPV)

  describe "Rendering" $ do
    it "renders minimal formative" $ do
      let f = minimalFormative "ml"
          rendered = renderFormative f
      rendered `shouldSatisfy` T.isInfixOf "ml"

    it "renderSlotIV round-trips with parseSlotIV for all contexts" $ do
      let testRoundTrip s = parseSlotIV (renderSlotIV s) `shouldBe` Just s
      testRoundTrip (STA, BSC, EXS)
      testRoundTrip (DYN, OBJ, EXS)
      testRoundTrip (STA, BSC, FNC)
      testRoundTrip (STA, OBJ, FNC)
      testRoundTrip (STA, BSC, RPS)
      testRoundTrip (STA, BSC, AMG)

    it "renderCase round-trips with parseCase" $ do
      let testRT c = parseCase (renderCase c) `shouldBe` Just c
      testRT (Transrelative THM)
      testRT (Transrelative ERG)
      testRT (Appositive GEN)
      testRT (Associative APL)
      testRT (Adverbial FUN)

  describe "Full Parsing" $ do
    it "parses a minimal consonant-initial formative" $ do
      case parseFormative "mal" of
        Success f -> do
          fSlotII f `shouldBe` (S1, PRC)
          fSlotIII f `shouldBe` Root "m"
        Failure e -> expectationFailure $ "Parse failed: " ++ show e

    it "parses a vowel-initial formative" $ do
      case parseFormative "emal" of
        Success f -> do
          fSlotII f `shouldBe` (S2, PRC)
          fSlotIII f `shouldBe` Root "m"
        Failure e -> expectationFailure $ "Parse failed: " ++ show e

    it "detects default stress as penultimate" $ do
      detectStress "mala" `shouldBe` Penultimate

    it "counts syllables" $ do
      countSyllables "mala" `shouldBe` 2
      countSyllables "malëuţřait" `shouldBe` 3

  describe "Glossing" $ do
    it "glosses a word with empty lexicon" $ do
      let result = glossWord Regular Map.empty Map.empty "mala"
      grWord result `shouldBe` "mala"
      grGloss result `shouldSatisfy` (not . T.null)
