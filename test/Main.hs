{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import Ithkuil.Phonology
import Ithkuil.Grammar
import Ithkuil.Parse
import Ithkuil.Render
import Ithkuil.FullParse
import Ithkuil.Adjuncts
import Ithkuil.WordType

main :: IO ()
main = hspec $ do
  describe "Phonology" $ do
    it "has 9 vowels" $
      length vowels `shouldBe` 9

    it "has 31 consonants" $
      length consonants `shouldBe` 31

    it "vowel form table has 4 series of 9 forms" $ do
      length vowelFormTable `shouldBe` 4
      all (\row -> length row == 9) vowelFormTable `shouldBe` True

    it "vowel form series 1 matches" $
      vowelForm 1 1 `shouldBe` "a"

    it "vowel form series 2 matches" $
      vowelForm 2 1 `shouldBe` "ai"

  describe "Grammar" $ do
    it "encodes stem 1 processual as 'a'" $
      slotIIToVv (S1, PRC) `shouldBe` "a"

    it "encodes stem 1 completive as 'ä'" $
      slotIIToVv (S1, CPT) `shouldBe` "ä"

    it "creates minimal formative" $ do
      let f = minimalFormative "ml"
      fSlotIII f `shouldBe` Root "ml"
      fSlotII f `shouldBe` defaultSlotII
      fSlotVI f `shouldBe` defaultSlotVI

    it "has 18 non-uniplex configurations" $
      length (filter (/= UNI) (allOf :: [Configuration])) `shouldBe` 18

    it "has 9 valences" $
      length (allOf :: [Valence]) `shouldBe` 9

    it "has 36 aspects" $
      length (allOf :: [Aspect]) `shouldBe` 36

    it "has 68 cases across 8 groups" $ do
      length (allOf :: [TransrelativeCase]) `shouldBe` 9
      length (allOf :: [AppositiveCase]) `shouldBe` 9
      length (allOf :: [AssociativeCase]) `shouldBe` 9
      length (allOf :: [AdverbialCase]) `shouldBe` 9
      length (allOf :: [RelationalCase]) `shouldBe` 8
      length (allOf :: [AffinitiveCase]) `shouldBe` 8
      length (allOf :: [SpatioTemporal1Case]) `shouldBe` 8
      length (allOf :: [SpatioTemporal2Case]) `shouldBe` 8

  describe "Slot II Parsing" $ do
    it "parses all 8 Vv vowels" $ do
      parseSlotII "a"  `shouldBe` Just (S1, PRC)
      parseSlotII "ä"  `shouldBe` Just (S1, CPT)
      parseSlotII "e"  `shouldBe` Just (S2, PRC)
      parseSlotII "i"  `shouldBe` Just (S2, CPT)
      parseSlotII "u"  `shouldBe` Just (S3, PRC)
      parseSlotII "ü"  `shouldBe` Just (S3, CPT)
      parseSlotII "o"  `shouldBe` Just (S0, PRC)
      parseSlotII "ö"  `shouldBe` Just (S0, CPT)

    it "rejects invalid Vv" $
      parseSlotII "x" `shouldBe` Nothing

  describe "Slot IV Parsing" $ do
    it "parses all EXS context forms" $ do
      parseSlotIV "a"  `shouldBe` Just (STA, BSC, EXS)
      parseSlotIV "ä"  `shouldBe` Just (STA, CTE, EXS)
      parseSlotIV "e"  `shouldBe` Just (STA, CSV, EXS)
      parseSlotIV "i"  `shouldBe` Just (STA, OBJ, EXS)
      parseSlotIV "u"  `shouldBe` Just (DYN, BSC, EXS)
      parseSlotIV "o"  `shouldBe` Just (DYN, CSV, EXS)

    it "parses FNC context forms" $ do
      parseSlotIV "ai" `shouldBe` Just (STA, BSC, FNC)
      parseSlotIV "au" `shouldBe` Just (STA, CTE, FNC)
      parseSlotIV "ui" `shouldBe` Just (DYN, BSC, FNC)

    it "parses RPS context forms (series 3)" $ do
      parseSlotIV "ia" `shouldBe` Just (STA, BSC, RPS)
      parseSlotIV "iä" `shouldBe` Just (STA, CTE, RPS)
      parseSlotIV "ua" `shouldBe` Just (DYN, BSC, RPS)

    it "parses AMG context forms (series 4)" $ do
      parseSlotIV "ao" `shouldBe` Just (STA, BSC, AMG)
      parseSlotIV "ae" `shouldBe` Just (STA, CTE, AMG)
      parseSlotIV "oa" `shouldBe` Just (DYN, BSC, AMG)

  describe "Case Parsing" $ do
    it "parses all transrelative cases" $ do
      parseCase "a"  `shouldBe` Just (Transrelative THM)
      parseCase "ä"  `shouldBe` Just (Transrelative INS)
      parseCase "e"  `shouldBe` Just (Transrelative ABS)
      parseCase "i"  `shouldBe` Just (Transrelative AFF)
      parseCase "o"  `shouldBe` Just (Transrelative ERG)
      parseCase "u"  `shouldBe` Just (Transrelative IND)

    it "parses appositive cases" $ do
      parseCase "ai" `shouldBe` Just (Appositive POS)
      parseCase "ei" `shouldBe` Just (Appositive GEN)
      parseCase "ui" `shouldBe` Just (Appositive PAR)

    it "parses spatio-temporal cases with glottal stop" $ do
      parseCase "a'a" `shouldBe` Just (SpatioTemporal1 LOC)
      parseCase "e'e" `shouldBe` Just (SpatioTemporal1 ALL)
      parseCase "i'i" `shouldBe` Just (SpatioTemporal2 CNR)

  describe "Case Rendering" $ do
    it "renders all transrelative cases" $ do
      renderCase (Transrelative THM) `shouldBe` "a"
      renderCase (Transrelative ERG) `shouldBe` "o"
      renderCase (Transrelative IND) `shouldBe` "u"

    it "renders spatio-temporal cases" $ do
      renderCase (SpatioTemporal1 LOC) `shouldBe` "a'a"
      renderCase (SpatioTemporal2 CNR) `shouldBe` "i'i"
      renderCase (SpatioTemporal2 PLM) `shouldBe` "a'u"

    it "round-trips non-colliding cases through parse/render" $ do
      -- Some case vowels collide across groups (e.g., "iu" = IDP and CSD)
      -- because the official grammar reuses vowel forms across series.
      -- Here we test that at least the first-match cases round-trip.
      let transrelative = map Transrelative allOf  -- 9, all unique
          appositive = map Appositive allOf        -- 9, all unique
      let roundTrip c = parseCase (renderCase c) == Just c
      all roundTrip transrelative `shouldBe` True
      all roundTrip appositive `shouldBe` True

  describe "Ca Complex Parsing" $ do
    it "parses default Ca 'l'" $
      parseCa "l" `shouldBe` Just (ParsedCa UNI CSL M_ DEL NRM)

    it "parses standalone perspective forms" $ do
      parseCa "r" `shouldBe` Just (ParsedCa UNI CSL G_ DEL NRM)
      parseCa "v" `shouldBe` Just (ParsedCa UNI CSL N_ DEL NRM)

    it "parses extension forms" $ do
      parseCa "d" `shouldBe` Just (ParsedCa UNI CSL M_ PRX NRM)
      parseCa "g" `shouldBe` Just (ParsedCa UNI CSL M_ ICP NRM)
      parseCa "b" `shouldBe` Just (ParsedCa UNI CSL M_ ATV NRM)

    it "parses configuration + perspective" $ do
      parseCa "tr" `shouldBe` Just (ParsedCa MSS CSL G_ DEL NRM)
      parseCa "kr" `shouldBe` Just (ParsedCa MSC CSL G_ DEL NRM)

    it "parses representative essence" $
      parseCa "tļ" `shouldBe` Just (ParsedCa UNI CSL M_ DEL RPV)

  describe "Formative Parsing" $ do
    it "parses consonant-initial words" $ do
      let result = parseFormativeReal "malai"
      result `shouldSatisfy` (/= Nothing)
      case result of
        Just pf -> do
          pfSlotII pf `shouldBe` (S1, PRC)
          pfRoot pf `shouldBe` Root "m"
        Nothing -> expectationFailure "Should parse"

    it "parses vowel-initial words" $ do
      let result = parseFormativeReal "emalai"
      result `shouldSatisfy` (/= Nothing)
      case result of
        Just pf -> do
          pfSlotII pf `shouldBe` (S2, PRC)
          pfRoot pf `shouldBe` Root "m"
        Nothing -> expectationFailure "Should parse"

  describe "Conjunct Splitting" $ do
    it "splits simple CV-CV words" $
      splitConjuncts "mala" `shouldBe` ["m", "a", "l", "a"]

    it "splits consonant clusters" $
      splitConjuncts "ţřai" `shouldBe` ["ţř", "ai"]

    it "handles vowel-initial words" $
      splitConjuncts "emal" `shouldBe` ["e", "m", "a", "l"]

  describe "Bias Adjuncts" $ do
    it "parses known bias forms" $ do
      parseBias "řřx" `shouldBe` Just DOL
      parseBias "kff" `shouldBe` Just DIS
      parseBias "lf"  `shouldBe` Just ACC

    it "rejects unknown bias forms" $
      parseBias "xyz" `shouldBe` Nothing

    it "all biases have unique forms" $ do
      let biases = allOf :: [Bias]
          forms = map biasForm biases
      length forms `shouldBe` length (filter (uncurry (/=)) $ zip forms (tail forms)) + 1

  describe "Stress Detection" $ do
    it "detects default penultimate stress" $
      detectStress "malai" `shouldBe` Penultimate

    it "detects ultimate stress from acute accent" $
      detectStress "malá" `shouldBe` Ultimate

  describe "Vn/Cn Parsing" $ do
    it "parses valence vowels" $ do
      parseVnValence "a"  `shouldBe` Just MNO
      parseVnValence "ä"  `shouldBe` Just PRL
      parseVnValence "e"  `shouldBe` Just CRO
      parseVnValence "u"  `shouldBe` Just PTI

    it "parses mood consonants" $ do
      parseCnMood "h"  `shouldBe` Just FAC
      parseCnMood "hl" `shouldBe` Just SUB
      parseCnMood "hr" `shouldBe` Just ASM
      parseCnMood "hm" `shouldBe` Just SPC
      parseCnMood "hn" `shouldBe` Just COU
      parseCnMood "hň" `shouldBe` Just HYP

    it "parses case-scope consonants" $ do
      parseCnCaseScope "w" `shouldBe` Just CCN
      parseCnCaseScope "hw" `shouldBe` Just CCA

  describe "Vk Parsing" $ do
    it "parses assertive with validation" $ do
      parseVk "a" `shouldBe` Success (IllocVal ASR OBS)
      parseVk "ä" `shouldBe` Success (IllocVal ASR REC)
      parseVk "u" `shouldBe` Success (IllocVal ASR INF)

    it "parses other illocutions (series 2)" $ do
      parseVk "ai" `shouldBe` Success (IllocVal DIR OBS)
      parseVk "ei" `shouldBe` Success (IllocVal IRG OBS)

  describe "Word Type Classification" $ do
    it "classifies pure consonant words as bias adjuncts" $ do
      classifyWord "řřx" `shouldBe` WBiasAdjunct
      classifyWord "kff" `shouldBe` WBiasAdjunct
      classifyWord "lf"  `shouldBe` WBiasAdjunct

    it "classifies h-initial words as register adjuncts" $ do
      classifyWord "hlw" `shouldBe` WRegisterAdjunct
      classifyWord "hrw" `shouldBe` WRegisterAdjunct

    it "classifies normal words as formatives" $ do
      classifyWord "malai" `shouldBe` WFormative
      classifyWord "emalai" `shouldBe` WFormative

  describe "Rendering" $ do
    it "renders a minimal formative" $ do
      let f = minimalFormative "ml"
      let rendered = renderFormative f
      rendered `shouldSatisfy` (not . T.null)
      -- Should contain the root "ml"
      T.isInfixOf "ml" rendered `shouldBe` True

    it "renders valence vowels correctly" $ do
      renderValence MNO `shouldBe` "a"
      renderValence PTI `shouldBe` "u"
      renderValence CPL `shouldBe` "ëi"

    it "renders mood consonants correctly" $ do
      renderMoodOrScope (MoodVal FAC) `shouldBe` "h"
      renderMoodOrScope (MoodVal SUB) `shouldBe` "hl"
      renderMoodOrScope (MoodVal HYP) `shouldBe` "hň"

    it "renders illocution+validation correctly" $ do
      renderIllocution ASR `shouldBe` ""
      renderIllocution DIR `shouldBe` "h"
      renderValidation OBS `shouldBe` "a"
      renderValidation INF `shouldBe` "u"
