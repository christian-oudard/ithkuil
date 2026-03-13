{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.Hspec
import qualified Data.Text as T
import Ithkuil.Phonology
import Ithkuil.Grammar
import Ithkuil.Parse
import Ithkuil.Render
import Ithkuil.FullParse
import Ithkuil.Adjuncts
import Ithkuil.WordType
import Ithkuil.Referentials

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
      let transrelative = map Transrelative allOf
          appositive = map Appositive allOf
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

    it "parses with non-default Ca" $ do
      let result = parseFormativeReal "maru"
      case result of
        Just pf -> do
          pfRoot pf `shouldBe` Root "m"
          -- "r" Ca = G perspective
          pfCaParsed pf `shouldBe` Just (ParsedCa UNI CSL G_ DEL NRM)
        Nothing -> expectationFailure "Should parse maru"

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
      length forms `shouldBe` length (filter (uncurry (/=)) $ zip forms (drop 1 forms)) + 1

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

    it "parses all 9 valences" $ do
      parseVnValence "a"  `shouldBe` Just MNO
      parseVnValence "ä"  `shouldBe` Just PRL
      parseVnValence "e"  `shouldBe` Just CRO
      parseVnValence "i"  `shouldBe` Just RCP
      parseVnValence "ëi" `shouldBe` Just CPL
      parseVnValence "ö"  `shouldBe` Just DUP
      parseVnValence "o"  `shouldBe` Just DEM
      parseVnValence "ü"  `shouldBe` Just CNG
      parseVnValence "u"  `shouldBe` Just PTI

    it "parses mood consonants" $ do
      parseCnMood "h"  `shouldBe` Just FAC
      parseCnMood "hl" `shouldBe` Just SUB
      parseCnMood "hr" `shouldBe` Just ASM
      parseCnMood "hm" `shouldBe` Just SPC
      parseCnMood "hn" `shouldBe` Just COU
      parseCnMood "hň" `shouldBe` Just HYP

    it "parses Pattern 2 mood consonants" $ do
      parseCnMoodP2 "w"   `shouldBe` Just FAC
      parseCnMoodP2 "y"   `shouldBe` Just FAC
      parseCnMoodP2 "hw"  `shouldBe` Just SUB
      parseCnMoodP2 "hrw" `shouldBe` Just ASM
      parseCnMoodP2 "hmw" `shouldBe` Just SPC
      parseCnMoodP2 "hnw" `shouldBe` Just COU
      parseCnMoodP2 "hňw" `shouldBe` Just HYP

    it "parses case-scope consonants" $ do
      parseCnCaseScope "w" `shouldBe` Just CCN
      parseCnCaseScope "hw" `shouldBe` Just CCA

    it "parses phase vowels" $ do
      lookup "ai" phaseVowels `shouldBe` Just PUN
      lookup "au" phaseVowels `shouldBe` Just ITR
      lookup "ei" phaseVowels `shouldBe` Just REP
      lookup "ui" phaseVowels `shouldBe` Just FLC

    it "parses aspect vowels (all 36)" $ do
      -- Column 1 (Series 1)
      lookup "a" aspectVowels `shouldBe` Just RTR
      lookup "e" aspectVowels `shouldBe` Just HAB
      lookup "u" aspectVowels `shouldBe` Just ATP
      -- Column 2 (Series 2)
      lookup "ai" aspectVowels `shouldBe` Just RSM
      lookup "ou" aspectVowels `shouldBe` Just CNT
      -- Column 3 (Series 3)
      lookup "ia" aspectVowels `shouldBe` Just PMP
      lookup "ua" aspectVowels `shouldBe` Just PPR
      -- Column 4 (Series 4)
      lookup "ao" aspectVowels `shouldBe` Just DCL
      lookup "oa" aspectVowels `shouldBe` Just SQN

  describe "Vk Parsing" $ do
    it "parses assertive with validation" $ do
      parseVk "a" `shouldBe` Success (IllocVal ASR OBS)
      parseVk "ä" `shouldBe` Success (IllocVal ASR REC)
      parseVk "u" `shouldBe` Success (IllocVal ASR INF)

    it "parses other illocutions (series 2)" $ do
      parseVk "ai" `shouldBe` Success (IllocVal DIR OBS)
      parseVk "ei" `shouldBe` Success (IllocVal IRG OBS)

  describe "Referentials" $ do
    it "has correct C1 forms for neutral effect" $ do
      refC1 (PersonalRef R1m NEU) `shouldBe` "l"   -- I
      refC1 (PersonalRef R2m NEU) `shouldBe` "s"   -- you sg.
      refC1 (PersonalRef R2p NEU) `shouldBe` "n"   -- you pl.
      refC1 (PersonalRef Rma NEU) `shouldBe` "m"   -- he/she
      refC1 (PersonalRef Rpa NEU) `shouldBe` "ň"   -- they (animate)
      refC1 (PersonalRef Rmi NEU) `shouldBe` "z"   -- it
      refC1 (PersonalRef Rmx NEU) `shouldBe` "c"   -- mixed

    it "has correct C1 forms for beneficial effect" $ do
      refC1 (PersonalRef R1m BEN) `shouldBe` "r"
      refC1 (PersonalRef R2m BEN) `shouldBe` "š"
      refC1 (PersonalRef Rma BEN) `shouldBe` "p"

    it "has correct C1 forms for detrimental effect" $ do
      refC1 (PersonalRef R1m DET) `shouldBe` "ř"
      refC1 (PersonalRef R2m DET) `shouldBe` "ž"
      refC1 (PersonalRef Rma DET) `shouldBe` "b"

    it "round-trips C1 lookup" $ do
      lookupRefC1 "l" `shouldBe` Just (PersonalRef R1m NEU)
      lookupRefC1 "s" `shouldBe` Just (PersonalRef R2m NEU)
      lookupRefC1 "m" `shouldBe` Just (PersonalRef Rma NEU)
      lookupRefC1 "r" `shouldBe` Just (PersonalRef R1m BEN)
      lookupRefC1 "xyz" `shouldBe` Nothing

    it "has 33 unique C1 forms (11 referents x 3 effects)" $ do
      let allForms = map snd refC1All
      length allForms `shouldBe` 33

  describe "Word Type Classification" $ do
    it "classifies pure consonant words as bias adjuncts" $ do
      classifyWord "řřx" `shouldBe` WBiasAdjunct
      classifyWord "kff" `shouldBe` WBiasAdjunct
      classifyWord "lf"  `shouldBe` WBiasAdjunct

    it "classifies h-initial words as register adjuncts" $ do
      classifyWord "hlw" `shouldBe` WRegisterAdjunct
      classifyWord "hrw" `shouldBe` WRegisterAdjunct

    it "classifies V-Cn patterns as modular adjuncts" $ do
      classifyWord "ah"  `shouldBe` WModularAdjunct
      classifyWord "aw"  `shouldBe` WModularAdjunct
      classifyWord "ehl" `shouldBe` WModularAdjunct

    it "classifies short C-V words as referentials" $ do
      classifyWord "la" `shouldBe` WReferential
      classifyWord "se" `shouldBe` WReferential
      classifyWord "mo" `shouldBe` WReferential

    it "classifies normal words as formatives" $ do
      classifyWord "malai" `shouldBe` WFormative
      classifyWord "emalai" `shouldBe` WFormative

  describe "Referential Parsing" $ do
    it "parses simple referential 'la' as 1m-THM" $ do
      let pw = parseWord "la"
      case pw of
        PReferential ref mc _ -> do
          ref `shouldBe` PersonalRef R1m NEU
          mc `shouldBe` Just (Transrelative THM)
        _ -> expectationFailure $ "Expected PReferential, got: " ++ show pw

    it "parses 'se' as 2m-ABS" $ do
      case parseWord "se" of
        PReferential ref mc _ -> do
          ref `shouldBe` PersonalRef R2m NEU
          mc `shouldBe` Just (Transrelative ABS)
        pw -> expectationFailure $ "Expected PReferential, got: " ++ show pw

    it "parses 'ro' as 1m/BEN-ERG" $ do
      case parseWord "ro" of
        PReferential ref mc _ -> do
          ref `shouldBe` PersonalRef R1m BEN
          mc `shouldBe` Just (Transrelative ERG)
        pw -> expectationFailure $ "Expected PReferential, got: " ++ show pw

  describe "Modular Adjunct Parsing" $ do
    it "parses 'ah' as MNO-FAC" $ do
      case parseWord "ah" of
        PModular [VnCnValence MNO (MoodVal FAC)] _ -> return ()
        pw -> expectationFailure $ "Expected PModular MNO-FAC, got: " ++ show pw

    it "parses 'aw' as RTR-FAC (aspect)" $ do
      case parseWord "aw" of
        PModular [VnCnAspect RTR (MoodVal FAC)] _ -> return ()
        pw -> expectationFailure $ "Expected PModular RTR-FAC, got: " ++ show pw

    it "parses 'ehl' as CRO-SUB" $ do
      case parseWord "ehl" of
        PModular [VnCnValence CRO (MoodVal SUB)] _ -> return ()
        pw -> expectationFailure $ "Expected PModular CRO-SUB, got: " ++ show pw

    it "parses aspect pattern 2 'aihňw' as RSM-HYP" $ do
      case parseWord "aihňw" of
        PModular [VnCnAspect RSM (MoodVal HYP)] _ -> return ()
        pw -> expectationFailure $ "Expected PModular RSM-HYP, got: " ++ show pw

  describe "Affix Extraction" $ do
    it "extracts no affixes from simple Ca" $ do
      extractAffixes ["l"] `shouldBe` []

    it "extracts one VxCs affix after Ca" $ do
      extractAffixes ["l", "e", "sp"] `shouldBe` [("e", "sp")]

    it "extracts multiple affixes" $ do
      extractAffixes ["l", "a", "sp", "i", "rs"] `shouldBe` [("a", "sp"), ("i", "rs")]

    it "classifies Type 1 degree vowels" $ do
      classifyDegree "a" `shouldBe` 1
      classifyDegree "e" `shouldBe` 3
      classifyDegree "ëi" `shouldBe` 5
      classifyDegree "u" `shouldBe` 9
      classifyDegree "ae" `shouldBe` 0

    it "classifies Type 2 degree vowels" $ do
      classifyDegree "ai" `shouldBe` 1
      classifyDegree "ei" `shouldBe` 3
      classifyDegree "ui" `shouldBe` 9

    it "classifies Type 3 degree vowels" $ do
      classifyDegree "ia" `shouldBe` 1
      classifyDegree "uä" `shouldBe` 1
      classifyDegree "eë" `shouldBe` 5
      classifyDegree "ua" `shouldBe` 9

  describe "Referent Labels" $ do
    it "provides human-readable labels" $ do
      referentLabel R1m `shouldBe` "I"
      referentLabel R2m `shouldBe` "you(sg.)"
      referentLabel R2p `shouldBe` "you(pl.)"
      referentLabel Rma `shouldBe` "he/she"
      referentLabel Rmi `shouldBe` "it"
      referentLabel Rpvs `shouldBe` "whatever"

  describe "Stress Detection in Formatives" $ do
    it "detects penultimate stress by default" $ do
      case parseFormativeReal "malai" of
        Just pf -> pfStress pf `shouldBe` Penultimate
        Nothing -> expectationFailure "Should parse"

    it "detects ultimate stress from acute accent" $ do
      case parseFormativeReal "malá" of
        Just pf -> pfStress pf `shouldBe` Ultimate
        Nothing -> expectationFailure "Should parse"

  describe "Rendering" $ do
    it "renders a minimal formative" $ do
      let f = minimalFormative "ml"
      let rendered = renderFormative f
      rendered `shouldSatisfy` (not . T.null)
      T.isInfixOf "ml" rendered `shouldBe` True

    it "renders valence vowels correctly" $ do
      renderValence MNO `shouldBe` "a"
      renderValence PTI `shouldBe` "u"
      renderValence CPL `shouldBe` "ëi"

    it "renders mood consonants correctly" $ do
      renderMoodOrScope (MoodVal FAC) `shouldBe` "h"
      renderMoodOrScope (MoodVal SUB) `shouldBe` "hl"
      renderMoodOrScope (MoodVal HYP) `shouldBe` "hň"

    it "renders Pattern 2 mood consonants" $ do
      renderMoodOrScopeP2 (MoodVal FAC) `shouldBe` "w"
      renderMoodOrScopeP2 (MoodVal SUB) `shouldBe` "hw"
      renderMoodOrScopeP2 (MoodVal HYP) `shouldBe` "hňw"

    it "renders phase vowels correctly" $ do
      renderPhase PUN `shouldBe` "ai"
      renderPhase FLC `shouldBe` "ui"

    it "renders aspect vowels correctly" $ do
      renderAspect RTR `shouldBe` "a"
      renderAspect RSM `shouldBe` "ai"
      renderAspect PMP `shouldBe` "ia"
      renderAspect DCL `shouldBe` "ao"
      renderAspect SQN `shouldBe` "oa"

    it "renders illocution+validation correctly" $ do
      renderIllocution ASR `shouldBe` ""
      renderIllocution DIR `shouldBe` "h"
      renderValidation OBS `shouldBe` "a"
      renderValidation INF `shouldBe` "u"
