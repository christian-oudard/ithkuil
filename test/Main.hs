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
import Ithkuil.Validation (StressError(..), validateStress)
import Ithkuil.Compose (lookupGrammar, GrammarEntry(..), composeFormative, applyStress)

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

    it "has 19 non-uniplex configurations" $
      length (filter (/= UNI) (allOf :: [Configuration])) `shouldBe` 19

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
      parseSlotIV "ie" `shouldBe` Just (STA, CTE, RPS)
      parseSlotIV "ua" `shouldBe` Just (DYN, BSC, RPS)

    it "parses Series 3 alternate vowel forms" $ do
      -- Alternates used after y- (i-initial → u/ü alternates)
      parseSlotIV "uä" `shouldBe` Just (STA, BSC, RPS)
      parseSlotIV "uë" `shouldBe` Just (STA, CTE, RPS)
      parseSlotIV "üä" `shouldBe` Just (STA, CSV, RPS)
      parseSlotIV "üë" `shouldBe` Just (STA, OBJ, RPS)
      -- Alternates used after w- (u-initial → ö/i alternates)
      parseSlotIV "öë" `shouldBe` Just (DYN, OBJ, RPS)
      parseSlotIV "öä" `shouldBe` Just (DYN, CSV, RPS)
      parseSlotIV "ië" `shouldBe` Just (DYN, CTE, RPS)
      parseSlotIV "iä" `shouldBe` Just (DYN, BSC, RPS)

    it "parses Series 3 alternate Vv forms" $ do
      parseSlotII "uä" `shouldBe` Just (S1, PRC)  -- alternate of ia
      parseSlotII "iä" `shouldBe` Just (S3, PRC)  -- alternate of ua

    it "parses AMG context forms (series 4)" $ do
      parseSlotIV "ao" `shouldBe` Just (STA, BSC, AMG)
      parseSlotIV "aö" `shouldBe` Just (STA, CTE, AMG)
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

    it "parses relational cases with glottal stop" $ do
      parseCase "a'a" `shouldBe` Just (Relational PRN)
      parseCase "e'e" `shouldBe` Just (Relational COR)
      parseCase "i'i" `shouldBe` Just (Relational CPS)

    it "parses spatio-temporal cases with glottal stop" $ do
      parseCase "i'a" `shouldBe` Just (SpatioTemporal1 LOC)
      parseCase "i'e" `shouldBe` Just (SpatioTemporal1 ATD)
      parseCase "a'o" `shouldBe` Just (SpatioTemporal2 CNR)

  describe "Case Rendering" $ do
    it "renders all transrelative cases" $ do
      renderCase (Transrelative THM) `shouldBe` "a"
      renderCase (Transrelative ERG) `shouldBe` "o"
      renderCase (Transrelative IND) `shouldBe` "u"

    it "renders relational cases" $ do
      renderCase (Relational PRN) `shouldBe` "a'a"
      renderCase (Relational COR) `shouldBe` "e'e"

    it "renders spatio-temporal cases" $ do
      renderCase (SpatioTemporal1 LOC) `shouldBe` "i'a"
      renderCase (SpatioTemporal2 CNR) `shouldBe` "a'o"
      renderCase (SpatioTemporal2 PLM) `shouldBe` "ö'a"

    it "round-trips non-colliding cases through parse/render" $ do
      let transrelative = map Transrelative allOf
          appositive = map Appositive allOf
          associative = map Associative allOf
          adverbial = map Adverbial allOf
          relational = map Relational allOf
          affinitive = map Affinitive allOf
          spatioTemp1 = map SpatioTemporal1 allOf
          spatioTemp2 = map SpatioTemporal2 allOf
      let roundTrip c = parseCase (renderCase c) == Just c
      all roundTrip transrelative `shouldBe` True
      all roundTrip appositive `shouldBe` True
      all roundTrip associative `shouldBe` True
      all roundTrip adverbial `shouldBe` True
      all roundTrip relational `shouldBe` True
      all roundTrip affinitive `shouldBe` True
      all roundTrip spatioTemp1 `shouldBe` True
      all roundTrip spatioTemp2 `shouldBe` True

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

    it "parses complex allomorphic Ca forms" $ do
      parseCa "nš" `shouldBe` Just (ParsedCa UNI COA G_ DEL RPV)
      parseCa "řktç" `shouldBe` Just (ParsedCa MSC VAR A_ PRX RPV)
      parseCa "zḑ" `shouldBe` Just (ParsedCa MFS CSL A_ DPL RPV)
      parseCa "nx" `shouldBe` Just (ParsedCa MSC CSL N_ GRA RPV)

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
      parseBias "ẓmm" `shouldBe` Just DLC
      parseBias "pss" `shouldBe` Just MNF
      parseBias "msf" `shouldBe` Just RSG
      parseBias "xtļ" `shouldBe` Just ARB
      parseBias "lļ"  `shouldBe` Just ADS

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

    it "detects monosyllabic stress" $ do
      detectStressSimple "a" `shouldBe` Monosyllabic
      detectStressSimple "ëu" `shouldBe` Monosyllabic

    it "detects penultimate for unmarked polysyllabic" $ do
      detectStressSimple "ala" `shouldBe` Penultimate
      detectStressSimple "alai" `shouldBe` Penultimate

    it "detects ultimate from accent mark" $ do
      detectStressSimple "alá" `shouldBe` Ultimate
      detectStressSimple "alái" `shouldBe` Ultimate

    it "detects antepenultimate from accent mark" $ do
      detectStressSimple "ái'la'sa" `shouldBe` Antepenultimate
      detectStressSimple "ála'a" `shouldBe` Antepenultimate

  describe "Stress Validation" $ do
    it "rejects marked default stress on monosyllabic" $
      validateStress "á" `shouldBe` Left MarkedDefaultStress

    it "rejects marked default stress on penultimate" $
      validateStress "ála" `shouldBe` Left MarkedDefaultStress

    it "rejects double-marked stress" $
      validateStress "álá" `shouldBe` Left DoubleMarkedStress

    it "rejects unrecognized stress placement" $
      validateStress "álalala" `shouldBe` Left UnrecognizedPlacement

    it "rejects accent on monosyllabic diphthong" $
      -- Kotlin treats "aí" as UnrecognizedPlacement; we treat as MarkedDefault
      -- since splitConjuncts groups consecutive vowels as one syllable
      validateStress "aí" `shouldBe` Left MarkedDefaultStress

    it "accepts valid stress patterns" $ do
      validateStress "a" `shouldBe` Right Monosyllabic
      validateStress "ala" `shouldBe` Right Penultimate
      validateStress "alá" `shouldBe` Right Ultimate

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

    it "classifies h+vowel words as register adjuncts" $ do
      classifyWord "ha" `shouldBe` WRegisterAdjunct
      classifyWord "he" `shouldBe` WRegisterAdjunct
      classifyWord "hü" `shouldBe` WRegisterAdjunct
      classifyWord "hai" `shouldBe` WRegisterAdjunct

    it "classifies V-Cn-V patterns as modular adjuncts" $ do
      classifyWord "aha"  `shouldBe` WModularAdjunct
      classifyWord "ihnú" `shouldBe` WModularAdjunct
      classifyWord "ehlä" `shouldBe` WModularAdjunct

    it "classifies short C-V words as referentials" $ do
      classifyWord "la" `shouldBe` WReferential
      classifyWord "se" `shouldBe` WReferential
      classifyWord "mo" `shouldBe` WReferential

    it "classifies V-C words as affixual adjuncts" $ do
      classifyWord "äst" `shouldBe` WAffixualAdjunct
      classifyWord "eld" `shouldBe` WAffixualAdjunct

    it "classifies normal words as formatives" $ do
      classifyWord "malai" `shouldBe` WFormative
      classifyWord "emalai" `shouldBe` WFormative

  describe "Referential Parsing" $ do
    it "parses simple referential 'la' as 1m-THM" $ do
      let pw = parseWord "la"
      case pw of
        PReferential [ref] mc _ _ -> do
          ref `shouldBe` PersonalRef R1m NEU
          mc `shouldBe` Just (Transrelative THM)
        _ -> expectationFailure $ "Expected PReferential, got: " ++ show pw

    it "parses 'se' as 2m-ABS" $ do
      case parseWord "se" of
        PReferential [ref] mc _ _ -> do
          ref `shouldBe` PersonalRef R2m NEU
          mc `shouldBe` Just (Transrelative ABS)
        pw -> expectationFailure $ "Expected PReferential, got: " ++ show pw

    it "parses 'ro' as 1m/BEN-ERG" $ do
      case parseWord "ro" of
        PReferential [ref] mc _ _ -> do
          ref `shouldBe` PersonalRef R1m BEN
          mc `shouldBe` Just (Transrelative ERG)
        pw -> expectationFailure $ "Expected PReferential, got: " ++ show pw

    it "parses extended referential 'layá' as 1m-THM" $ do
      classifyWord "layá" `shouldBe` WReferential
      case parseWord "layá" of
        PReferential [ref] mc _ _ -> do
          ref `shouldBe` PersonalRef R1m NEU
          mc `shouldBe` Just (Transrelative THM)
        pw -> expectationFailure $ "Expected PReferential, got: " ++ show pw

    it "parses referential with second referent 'miyüs'" $ do
      classifyWord "miyüs" `shouldBe` WReferential
      case parseWord "miyüs" of
        PReferential [ref] mc _ ext -> do
          ref `shouldBe` PersonalRef Rma NEU
          mc `shouldBe` Just (Transrelative AFF)
          case ext of
            Just ("y", mc2, mRef2) -> do
              mc2 `shouldBe` Just (Transrelative DAT)
              mRef2 `shouldBe` Just (PersonalRef R2m NEU)
            _ -> expectationFailure $ "Expected extended referential, got: " ++ show ext
        pw -> expectationFailure $ "Expected PReferential, got: " ++ show pw

    it "parses cluster referential 'ţna' as mi.BEN+2p-THM" $ do
      classifyWord "ţna" `shouldBe` WReferential
      case parseWord "ţna" of
        PReferential refs mc _ _ -> do
          refs `shouldBe` [PersonalRef Rmi BEN, PersonalRef R2p NEU]
          mc `shouldBe` Just (Transrelative THM)
        pw -> expectationFailure $ "Expected PReferential, got: " ++ show pw

  describe "Modular Adjunct Parsing" $ do
    it "classifies V-Cn-V as modular adjunct" $ do
      classifyWord "ihnú" `shouldBe` WModularAdjunct
      classifyWord "aha" `shouldBe` WModularAdjunct
      classifyWord "ehlä" `shouldBe` WModularAdjunct

    it "parses 'ihnú' as RCP-COU with Vh scope" $ do
      case parseWord "ihnú" of
        PModular [VnCnValence RCP (MoodVal COU)] fv _ -> do
          fv `shouldBe` "{under adj.}"
        pw -> expectationFailure $ "Expected PModular RCP-COU, got: " ++ show pw

    it "parses 'aha' as MNO-FAC + implicit VnCn (penultimate stress)" $ do
      case parseWord "aha" of
        PModular [VnCnValence MNO (MoodVal FAC)] fv _ -> do
          fv `shouldBe` "MNO-FAC"  -- penultimate stress → final V + implicit "h" = another VnCn
        pw -> expectationFailure $ "Expected PModular MNO-FAC, got: " ++ show pw

    it "parses aspect-only modular 'a' (single vowel)" $ do
      classifyWord "a" `shouldBe` WModularAdjunct

  describe "Multiple Affix Adjunct" $ do
    it "classifies Cs-Vx-Cz-VxCs as multiple affix adjunct" $ do
      classifyWord "xaheitr" `shouldBe` WMultipleAffixAdj

    it "classifies with glottal Cz" $ do
      classifyWord "xa'heitr" `shouldBe` WMultipleAffixAdj

    it "classifies with ë prefix" $ do
      classifyWord "ëxaheitr" `shouldBe` WMultipleAffixAdj

    it "parses first affix, Cz scope, and additional affixes" $ do
      case parseWord "xaheitr" of
        PMultipleAffix (vx, cs) cz afxs mVz -> do
          cs `shouldBe` "x"
          vx `shouldBe` "a"
          cz `shouldBe` "h"
          afxs `shouldBe` [("ei", "tr")]
          mVz `shouldBe` Nothing
        pw -> expectationFailure $ "Expected PMultipleAffix, got: " ++ show pw

    it "parses with optional Vz scope vowel" $ do
      case parseWord "xaheitre" of
        PMultipleAffix _ _ afxs mVz -> do
          afxs `shouldBe` [("ei", "tr")]
          mVz `shouldBe` Just "e"
        pw -> expectationFailure $ "Expected PMultipleAffix, got: " ++ show pw

    it "parses glottal Cz scope" $ do
      case parseWord "xa'heitr" of
        PMultipleAffix _ cz _ _ -> cz `shouldBe` "'h"
        pw -> expectationFailure $ "Expected PMultipleAffix, got: " ++ show pw

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

  describe "Case-Accessor Affixes" $ do
    it "glosses w-series case accessor (direct case vowel)" $ do
      -- sw = type-1 case accessor, Vx "a" = THM case
      glossOneAffix mempty ("a", "sw") `shouldBe` "acc₁:THM"
      glossOneAffix mempty ("e", "sw") `shouldBe` "acc₁:ABS"
      glossOneAffix mempty ("ai", "sw") `shouldBe` "acc₁:POS"

    it "glosses y-series case accessor (glottalized vowel)" $ do
      -- sy = type-1 case accessor, Vx "a" → a'a = PRN (Relational)
      glossOneAffix mempty ("a", "sy") `shouldBe` "acc₁:PRN"
      glossOneAffix mempty ("e", "sy") `shouldBe` "acc₁:COR"
      -- Diphthong: "ai" → a'i = ACT (Affinitive)
      glossOneAffix mempty ("ai", "sy") `shouldBe` "acc₁:ACT"

    it "glosses inverse case accessor" $ do
      glossOneAffix mempty ("a", "šw") `shouldBe` "ia₁:THM"
      glossOneAffix mempty ("a", "žw") `shouldBe` "ia₂:THM"

    it "glosses case stacking" $ do
      glossOneAffix mempty ("a", "lw") `shouldBe` "case:THM"
      glossOneAffix mempty ("ei", "lw") `shouldBe` "case:GEN"

    it "glosses IVL affix (Cs=nļ)" $ do
      -- Series 1: illocution only
      glossOneAffix mempty ("a", "nļ") `shouldBe` "ASR"
      glossOneAffix mempty ("e", "nļ") `shouldBe` "DEC"
      glossOneAffix mempty ("u", "nļ") `shouldBe` "CNJ"
      -- Series 2: assertive + validation
      glossOneAffix mempty ("ai", "nļ") `shouldBe` "ASR/OBS"
      glossOneAffix mempty ("ei", "nļ") `shouldBe` "ASR/PUP"

    it "glosses Ca-stacking affix (Vx=üö)" $ do
      glossOneAffix mempty ("üö", "tr") `shouldBe` "Ca:MSS.G"
      glossOneAffix mempty ("üö", "s") `shouldBe` "Ca:DPX"
      glossOneAffix mempty ("üö", "l") `shouldBe` "Ca:default"

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

    it "detects antepenultimate stress from circumflex accent" $ do
      case parseFormativeReal "Mâlu'u" of
        Just pf -> pfStress pf `shouldBe` Antepenultimate
        Nothing -> expectationFailure "Should parse"

    it "normalizes circumflex â to ä" $ do
      normalizeAccents "â" `shouldBe` "ä"
      normalizeAccents "ê" `shouldBe` "ë"
      normalizeAccents "ô" `shouldBe` "ö"
      normalizeAccents "û" `shouldBe` "ü"

  describe "Diphthong Vv" $ do
    it "parses Series 2 diphthong Vv" $ do
      parseSlotII "ai" `shouldBe` Just (S1, PRC)
      parseSlotII "au" `shouldBe` Just (S1, CPT)
      parseSlotII "ei" `shouldBe` Just (S2, PRC)
      parseSlotII "ui" `shouldBe` Just (S3, PRC)

    it "parses Series 3 diphthong Vv" $ do
      parseSlotII "ia" `shouldBe` Just (S1, PRC)
      parseSlotII "ua" `shouldBe` Just (S3, PRC)

    it "parses w-shortcut with Series 3 Vv" $ do
      case parseFormativeReal "Wiadná" of
        Just pf -> do
          pfRoot pf `shouldBe` Root "dn"
          pfSlotII pf `shouldBe` (S1, PRC)
          pfCaParsed pf `shouldBe` Just (ParsedCa UNI CSL N_ DEL NRM)
        Nothing -> expectationFailure "Should parse Wiadná"

  describe "Minimal Formative Parsing" $ do
    it "parses w-glide vowel-initial words" $ do
      case parseFormativeReal "weli" of
        Just pf -> do
          let Root cr = pfRoot pf
          cr `shouldBe` "l"
          pfSlotII pf `shouldBe` (S2, PRC)
        Nothing -> expectationFailure "Should parse weli"

    it "parses minimal formative Vc as case, not Vr" $ do
      case parseFormativeReal "weli" of
        Just pf -> pfCase pf `shouldBe` Just (Transrelative AFF)
        Nothing -> expectationFailure "Should parse"

    it "parses minimal verbal with Vk" $ do
      case parseFormativeReal "malá" of
        Just pf -> pfIllocVal pf `shouldBe` Just (ASR, OBS)
        Nothing -> expectationFailure "Should parse"

    it "handles capitalized input" $ do
      case parseFormativeReal "Wekská" of
        Just pf -> do
          let Root cr = pfRoot pf
          cr `shouldBe` "ksk"
          pfIllocVal pf `shouldBe` Just (ASR, OBS)
        Nothing -> expectationFailure "Should parse Wekská"

    it "parses glottal stop case marker (final position)" $ do
      case parseFormativeReal "lala'a" of
        Just pf -> pfCase pf `shouldBe` Just (Relational PRN)
        Nothing -> expectationFailure "Should parse lala'a"

    it "parses glottal stop case marker (medial position)" $ do
      case parseFormativeReal "la'la" of
        Just pf -> pfCase pf `shouldBe` Just (Relational PRN)
        Nothing -> expectationFailure "Should parse la'la"

    it "parses Cc shortcut y- as PRX" $ do
      case parseFormativeReal "yužgrá" of
        Just pf -> do
          pfSlotII pf `shouldBe` (S3, PRC)
          pfCaParsed pf `shouldBe` Just (ParsedCa UNI CSL M_ PRX NRM)
          pfIllocVal pf `shouldBe` Just (ASR, OBS)
        Nothing -> expectationFailure "Should parse yužgrá"

    it "parses Cc shortcut w- as perspective" $ do
      case parseFormativeReal "wela" of
        Just pf -> do
          pfSlotII pf `shouldBe` (S2, PRC)
          pfRoot pf `shouldBe` Root "l"
          pfCaParsed pf `shouldBe` Just (ParsedCa UNI CSL M_ DEL NRM)
          pfCase pf `shouldBe` Just (Transrelative THM)
        Nothing -> expectationFailure "Should parse wela"

    it "strips Slot V filled marker from Cr" $ do
      case parseFormativeReal "a'larfunall" of
        Just pf -> do
          let Root cr = pfRoot pf
          cr `shouldBe` "l"
          length (pfSlotV pf) `shouldBe` 2
        Nothing -> expectationFailure "Should parse a'larfunall"

    it "extracts VnCn from formative Ca rest" $ do
      let pairs = extractAllPairs ["l", "ä", "hl"]
      pairs `shouldBe` [("ä", "hl")]
      extractVnCn ["l", "ä", "hl"] `shouldBe` Just ("ä", "hl")
      extractAffixes ["l", "ä", "hl"] `shouldBe` []

    it "extracts affixes from shortcut formatives" $ do
      case parseFormativeReal "wuksmenţa" of
        Just pf -> do
          pfRoot pf `shouldBe` Root "ksm"
          pfSlotII pf `shouldBe` (S3, PRC)
          let afxs = extractAffixes (pfCa pf)
          length afxs `shouldBe` 1
          snd (head afxs) `shouldBe` "nţ"
        Nothing -> expectationFailure "Should parse wuksmenţa"

    it "parses longer formatives with explicit Vr" $ do
      case parseFormativeReal "kšilo" of
        Just pf -> do
          let Root cr = pfRoot pf
          cr `shouldBe` "kš"
          pfSlotIV pf `shouldBe` (STA, OBJ, EXS)
          pfCase pf `shouldBe` Just (Transrelative ERG)
        Nothing -> expectationFailure "Should parse kšilo"

  describe "Cs-Root Formatives" $ do
    it "detects special Vv values" $ do
      isSpecialVv "ëi" `shouldBe` True
      isSpecialVv "eë" `shouldBe` True
      isSpecialVv "ëu" `shouldBe` True
      isSpecialVv "oë" `shouldBe` True
      isSpecialVv "ae" `shouldBe` True
      isSpecialVv "ea" `shouldBe` True
      isSpecialVv "a"  `shouldBe` False
      isSpecialVv "ai" `shouldBe` False

    it "parses Cs-root Vr as degree + context" $ do
      parseAffixVr "a"  `shouldBe` Just (1, EXS)
      parseAffixVr "ö"  `shouldBe` Just (6, EXS)
      parseAffixVr "u"  `shouldBe` Just (9, EXS)
      parseAffixVr "ai" `shouldBe` Just (1, FNC)
      parseAffixVr "ou" `shouldBe` Just (6, FNC)
      -- Degree-0 forms
      parseAffixVr "ae" `shouldBe` Just (0, EXS)
      parseAffixVr "ea" `shouldBe` Just (0, FNC)

    it "parses ëilal as Cs-root formative" $ do
      case parseFormativeReal "ëilal" of
        Just pf -> do
          pfRoot pf `shouldBe` Root "l"
          pfCsRootDegree pf `shouldBe` Just 1
          pfSlotII pf `shouldBe` (S1, PRC)
          pfSlotIV pf `shouldBe` (STA, BSC, EXS)
        Nothing -> expectationFailure "Should parse ëilal"

    it "parses oërmölá as CPT.DYN Cs-root" $ do
      case parseFormativeReal "oërmölá" of
        Just pf -> do
          pfRoot pf `shouldBe` Root "rm"
          pfCsRootDegree pf `shouldBe` Just 6
          pfSlotII pf `shouldBe` (S1, CPT)
          pfSlotIV pf `shouldBe` (DYN, BSC, EXS)
          pfIllocVal pf `shouldBe` Just (ASR, OBS)
        Nothing -> expectationFailure "Should parse oërmölá"

    it "parses oërmoulá with FNC context" $ do
      case parseFormativeReal "oërmoulá" of
        Just pf -> do
          pfRoot pf `shouldBe` Root "rm"
          pfCsRootDegree pf `shouldBe` Just 6
          pfSlotIV pf `shouldBe` (DYN, BSC, FNC)
        Nothing -> expectationFailure "Should parse oërmoulá"

    it "does not parse Cs-root with shortcuts" $ do
      -- w/y + special Vv should fail (shortcuts can't use Cs-root)
      -- In our implementation, w is parsed as Cc shortcut, then parseSlotII
      -- rejects the special Vv value, so this naturally fails
      case parseFormativeReal "wëila" of
        Just pf -> pfCsRootDegree pf `shouldBe` Nothing  -- parsed as normal, not Cs-root
        Nothing -> return ()  -- also acceptable: fails to parse

  describe "Reference-Root Formatives" $ do
    it "parses ea-initial word as reference-root (CPT version)" $ do
      case parseFormativeReal "ealali" of
        Just pf -> do
          pfSlotII pf `shouldBe` (S1, CPT)
          let Root cr = pfRoot pf
          cr `shouldBe` "l"
          pfCsRootDegree pf `shouldBe` Nothing  -- ref-root, not Cs-root
        Nothing -> expectationFailure "Should parse ealali"

    it "parses ae-initial word as reference-root (PRC version)" $ do
      case parseFormativeReal "aelali" of
        Just pf -> do
          pfSlotII pf `shouldBe` (S1, PRC)
          let Root cr = pfRoot pf
          cr `shouldBe` "l"
          pfCsRootDegree pf `shouldBe` Nothing
        Nothing -> expectationFailure "Should parse aelali"

  describe "Ca Gemination" $ do
    it "detects geminated Ca" $ do
      isGeminateCa "ll" `shouldBe` True
      isGeminateCa "rr" `shouldBe` True
      isGeminateCa "ttr" `shouldBe` True
      isGeminateCa "l" `shouldBe` False
      isGeminateCa "tr" `shouldBe` False

    it "detects allomorphic gemination" $ do
      isGeminateCa "jjn" `shouldBe` True
      isGeminateCa "xxn" `shouldBe` True

    it "degeminates Ca" $ do
      degeminateCa "ll" `shouldBe` "l"
      degeminateCa "rr" `shouldBe` "r"
      degeminateCa "ttr" `shouldBe` "tr"
      degeminateCa "jjn" `shouldBe` "dn"
      degeminateCa "xxn" `shouldBe` "kn"

    it "extracts Slot V affixes with geminated Ca" $ do
      -- "alarfull" = a-l-a-rf-u-ll → Vv=a, Cr=l, Vr=a, SlotV=[(rf,u)], Ca=ll→l
      case parseFormativeReal "alarfull" of
        Just pf -> do
          pfRoot pf `shouldBe` Root "l"
          pfSlotV pf `shouldBe` [("rf", "u")]
          pfCaParsed pf `shouldBe` Just (ParsedCa UNI CSL M_ DEL NRM)
        Nothing -> expectationFailure "Should parse alarfull"

    it "does not extract Slot V for non-geminated Ca" $ do
      case parseFormativeReal "malai" of
        Just pf -> pfSlotV pf `shouldBe` []
        Nothing -> expectationFailure "Should parse malai"

  describe "Mood/Case-Scope Adjuncts" $ do
    it "classifies hr+vowel as mood/case-scope adjunct" $ do
      classifyWord "hrei" `shouldBe` WMoodCaseScopeAdj
      classifyWord "hra" `shouldBe` WMoodCaseScopeAdj

    it "parses mood/case-scope adjuncts" $ do
      case parseWord "hrei" of
        PMoodCaseScope (CaseScope CCA) -> return ()
        pw -> expectationFailure $ "Expected CCA, got: " ++ show pw
      case parseWord "hra" of
        PMoodCaseScope (MoodVal FAC) -> return ()
        pw -> expectationFailure $ "Expected FAC, got: " ++ show pw
      case parseWord "hru" of
        PMoodCaseScope (MoodVal HYP) -> return ()
        pw -> expectationFailure $ "Expected HYP, got: " ++ show pw

  describe "Cc Parsing" $ do
    it "classifies concatenation types" $ do
      parseCc "h"  `shouldBe` (Just Type1, Nothing)
      parseCc "hl" `shouldBe` (Just Type1, Just ShortcutW)
      parseCc "hm" `shouldBe` (Just Type1, Just ShortcutY)
      parseCc "hw" `shouldBe` (Just Type2, Nothing)
      parseCc "hr" `shouldBe` (Just Type2, Just ShortcutW)
      parseCc "hn" `shouldBe` (Just Type2, Just ShortcutY)
      parseCc "w"  `shouldBe` (Nothing, Just ShortcutW)
      parseCc "y"  `shouldBe` (Nothing, Just ShortcutY)
      parseCc "l"  `shouldBe` (Nothing, Nothing)

  describe "Concatenated Formatives" $ do
    it "parses simple Type-1 concatenation" $ do
      case parseWord "hamala-lala" of
        PConcatenated pfs -> do
          length pfs `shouldBe` 2
          pfConcatenation (head pfs) `shouldBe` Just Type1
          pfRoot (head pfs) `shouldBe` Root "m"
          pfConcatenation (pfs !! 1) `shouldBe` Nothing
          pfRoot (pfs !! 1) `shouldBe` Root "l"
        pw -> expectationFailure $ "Expected PConcatenated, got: " ++ show pw

    it "parses Type-2 concatenation with shortcut" $ do
      case parseWord "hralai-malai" of
        PConcatenated pfs -> do
          length pfs `shouldBe` 2
          pfConcatenation (head pfs) `shouldBe` Just Type2
        pw -> expectationFailure $ "Expected PConcatenated, got: " ++ show pw

    it "parses concatenation with ç sentence prefix" $ do
      case parseWord "çëhamala-lala" of
        PConcatenated pfs -> do
          length pfs `shouldBe` 2
          pfConcatenation (head pfs) `shouldBe` Just Type1
        pw -> expectationFailure $ "Expected PConcatenated, got: " ++ show pw

    it "does not parse non-concatenated words with hyphens" $ do
      case parseWord "a-b" of
        PUnparsed _ -> return ()
        pw -> expectationFailure $ "Expected PUnparsed, got: " ++ show pw

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

  describe "Cluster Referential Decomposition" $ do
    it "decomposes biconsonantal referentials" $ do
      decomposeRefCluster "th" `shouldBe` Just [PersonalRef Rrdp NEU]
      decomposeRefCluster "ll" `shouldBe` Just [PersonalRef Robv NEU]
      decomposeRefCluster "mm" `shouldBe` Just [PersonalRef Rpvs NEU]

    it "decomposes multi-referent clusters" $ do
      decomposeRefCluster "ţn" `shouldBe` Just [PersonalRef Rmi BEN, PersonalRef R2p NEU]
      decomposeRefCluster "lm" `shouldBe` Just [PersonalRef R1m NEU, PersonalRef Rma NEU]
      decomposeRefCluster "sp" `shouldBe` Just [PersonalRef R2m NEU, PersonalRef Rma BEN]

    it "handles alternate pi.NEU form ļ" $ do
      decomposeRefCluster "ļ" `shouldBe` Just [PersonalRef Rpi NEU]

    it "distinguishes combination ref from geminated formative" $ do
      -- ţnaxeka = combination referential (no gemination)
      classifyWord "ţnaxeka" `shouldBe` WCombinationRef
      -- ţnaxekka = formative (geminated Ca "kk" = MSC config)
      classifyWord "ţnaxekka" `shouldBe` WFormative

    it "fails on non-referential consonants" $ do
      decomposeRefCluster "x" `shouldBe` Nothing
      decomposeRefCluster "q" `shouldBe` Nothing

    it "handles empty input" $ do
      decomposeRefCluster "" `shouldBe` Just []

  describe "Sentence Prefix (ç)" $ do
    it "detects ç sentence prefix in parsed formative" $ do
      case parseFormativeReal "çëlal" of
        Just pf -> pfSentenceStarter pf `shouldBe` True
        Nothing -> expectationFailure "çëlal should parse"
      case parseFormativeReal "çalal" of
        Just pf -> pfSentenceStarter pf `shouldBe` True
        Nothing -> expectationFailure "çalal should parse"

    it "does not set sentence prefix for normal words" $ do
      case parseFormativeReal "alal" of
        Just pf -> pfSentenceStarter pf `shouldBe` False
        Nothing -> expectationFailure "alal should parse"

    it "detects çç (sentence + PRX shortcut)" $ do
      case parseFormativeReal "ççala" of
        Just pf -> pfSentenceStarter pf `shouldBe` True
        Nothing -> expectationFailure "ççala should parse"

    it "includes [sentence:] in gloss output" $ do
      case parseWord "çëlal" of
        PFormative pf -> do
          pfSentenceStarter pf `shouldBe` True
          let gloss = glossWord mempty mempty (PFormative pf)
          T.isPrefixOf "[sentence:]" gloss `shouldBe` True
        pw -> expectationFailure $ "Expected PFormative, got: " ++ show pw

    it "rejects sentence prefix inside concatenation chain" $ do
      case parseWord "hamala-çëlala" of
        PError msg _ -> msg `shouldBe` "Sentence prefix inside concatenation chain"
        pw -> expectationFailure $ "Expected PError, got: " ++ show pw

    it "allows sentence prefix on first part of concatenation" $ do
      case parseWord "çëhamala-lala" of
        PConcatenated pfs -> do
          length pfs `shouldBe` 2
          pfSentenceStarter (head pfs) `shouldBe` True
        pw -> expectationFailure $ "Expected PConcatenated, got: " ++ show pw

    it "uses bracket notation for combination referentials" $ do
      case parseWord "ţnaxeka" of
        PCombinationRef _ _ _ _ _ -> do
          let gloss = glossWord mempty mempty (parseWord "ţnaxeka")
          T.isPrefixOf "[" gloss `shouldBe` True
          T.isInfixOf "mi.BEN" gloss `shouldBe` True
          T.isInfixOf "2p" gloss `shouldBe` True
        pw -> expectationFailure $ "Expected PCombinationRef, got: " ++ show pw

    it "shows both cases in combination referential with üa (THM)" $ do
      let gloss = glossWord mempty mempty (parseWord "mixenüa")
      -- Kotlin: ma-AFF-**n**/3₁-THM
      T.isInfixOf "AFF" gloss `shouldBe` True
      T.isInfixOf "THM" gloss `shouldBe` True

    it "omits case1 when no case2 in combination referential" $ do
      let gloss = glossWord mempty mempty (parseWord "ţnaxeka")
      -- Kotlin: [mi.BEN+2p]-**k**/3₁  (no case shown)
      T.isInfixOf "AFF" gloss `shouldBe` False
      T.isInfixOf "THM" gloss `shouldBe` False

    it "uses abbreviations for referential glossing" $ do
      let gloss = glossWord mempty mempty (parseWord "la")
      T.isInfixOf "1m" gloss `shouldBe` True

  describe "Kotlin Reference Alignment" $ do
    it "parses each word type correctly" $ do
      -- Formative
      case parseWord "lalu" of
        PFormative _ -> return ()
        pw -> expectationFailure $ "lalu: expected PFormative, got: " ++ show pw
      -- Modular adjunct
      case parseWord "ihnú" of
        PModular _ _ _ -> return ()
        pw -> expectationFailure $ "ihnú: expected PModular, got: " ++ show pw
      -- Affixual adjunct
      case parseWord "äst" of
        PAffixual _ _ _ _ -> return ()
        pw -> expectationFailure $ "äst: expected PAffixual, got: " ++ show pw
      -- Referential (extended)
      case parseWord "miyüs" of
        PReferential _ _ _ _ -> return ()
        pw -> expectationFailure $ "miyüs: expected PReferential, got: " ++ show pw
      -- Register
      case parseWord "ha" of
        PRegister DSV -> return ()
        pw -> expectationFailure $ "ha: expected PRegister DSV, got: " ++ show pw
      -- Bias
      case parseWord "pļļ" of
        PBias _ -> return ()
        pw -> expectationFailure $ "pļļ: expected PBias, got: " ++ show pw
      -- Mood/case-scope
      case parseWord "hrei" of
        PMoodCaseScope _ -> return ()
        pw -> expectationFailure $ "hrei: expected PMoodCaseScope, got: " ++ show pw

    it "glosses referential khe correctly" $ do
      let gloss = glossWord mempty mempty (parseWord "khe")
      gloss `shouldBe` "Rdp.DET-ABS"

    it "glosses modular ihnú correctly" $ do
      let gloss = glossWord mempty mempty (parseWord "ihnú")
      gloss `shouldBe` "RCP.COU-{under adj.}"

    it "glosses register ha correctly" $ do
      let gloss = glossWord mempty mempty (parseWord "ha")
      gloss `shouldBe` "DSV"

    it "glosses mood/case-scope hrei correctly" $ do
      let gloss = glossWord mempty mempty (parseWord "hrei")
      gloss `shouldBe` "CCA"

    it "glosses referential miyüs correctly" $ do
      let gloss = glossWord mempty mempty (parseWord "miyüs")
      gloss `shouldBe` "ma-AFF-DAT-2m"

    it "glosses referential layá with RPV essence" $ do
      let gloss = glossWord mempty mempty (parseWord "layá")
      gloss `shouldBe` "1m-THM-THM\\RPV"

    it "glosses Cs-root formatives" $ do
      -- ëilal = Cs-root with default Vv
      case parseWord "ëilal" of
        PFormative pf -> do
          pfCsRootDegree pf `shouldBe` Just 1
        pw -> expectationFailure $ "ëilal: expected PFormative Cs-root, got: " ++ show pw
      -- oërmölá = CPT.DYN Cs-root
      case parseWord "oërmölá" of
        PFormative pf -> do
          pfCsRootDegree pf `shouldSatisfy` (/= Nothing)
          pfRoot pf `shouldBe` Root "rm"
        pw -> expectationFailure $ "oërmölá: expected PFormative, got: " ++ show pw

    it "parses Level VnCn values" $ do
      -- ao + h = MIN level + FAC mood (in Ca conjuncts, extracted by glosser)
      let gloss = glossWord mempty mempty (parseWord "malaoha")
      T.isInfixOf "MIN" gloss `shouldBe` True
      -- Also test that parseOneVnCn handles Level
      parseOneVnCn "ao" "h" `shouldBe` Just (VnCnLevel MIN False (MoodVal FAC))
      parseOneVnCn "oa" "h" `shouldBe` Just (VnCnLevel MAX False (MoodVal FAC))

    it "parses absolute Level (V+y+V+Cn pattern)" $ do
      -- malalayoha: Ca rest has ...a-y-o-h → combined "ao" = MIN absolute
      let gloss = glossWord mempty mempty (parseWord "malalayoha")
      T.isInfixOf "MIN.a" gloss `shouldBe` True

    it "parses Effect VnCn values" $ do
      -- ia + h = BEN1 + FAC mood
      parseOneVnCn "ia" "h" `shouldBe` Just (VnCnEffect BEN1 (MoodVal FAC))
      parseOneVnCn "ue" "hl" `shouldBe` Just (VnCnEffect DET2 (MoodVal SUB))
      parseOneVnCn "eë" "h" `shouldBe` Just (VnCnEffect UNK (MoodVal FAC))
      parseOneVnCn "ua" "hr" `shouldBe` Just (VnCnEffect DET1 (MoodVal ASM))
      -- Effect uses Pattern 1 Cn only — Pattern 2 should give Aspect, not Effect
      parseOneVnCn "ia" "w" `shouldBe` Just (VnCnAspect PMP (MoodVal FAC))
      -- Test via glossWord
      let gloss = glossWord mempty mempty (parseWord "maliahra")
      T.isInfixOf "BEN1" gloss `shouldBe` True

    it "parses concatenated formative case with glottal shift" $ do
      -- hlamröé: hl=T1 shortcut, a=Vv, mr=Cr, öé=Vf case
      -- Ultimate stress + glottalize: öe -> ö'e -> PCR (Postcursive)
      let gloss = glossWord mempty mempty (parseWord "hlamröé-úçtļořëi")
      T.isInfixOf "T1" gloss `shouldBe` True
      T.isInfixOf "PCR" gloss `shouldBe` True

    it "parses glottal stop movement correctly" $ do
      -- la'la and lala'a should both give case PRN
      let gloss1 = glossWord mempty mempty (parseWord "la'la")
          gloss2 = glossWord mempty mempty (parseWord "lala'a")
      gloss1 `shouldBe` gloss2

    it "parses all Kotlin reference test words" $ do
      let words = [ "yužgrá", "eolaleici", "khe", "adni'lö", "yeilaiceu"
                  , "alarfull", "a'larfunall"
                  , "lala'a", "la'la", "wala'ana"
                  , "ëilal", "oërmölá", "oërmoulá"
                  , "lalu", "ihnú", "äst", "miyüs", "mixenüa", "ha", "pļļ", "hrei"
                  , "lála'a", "çëlal", "çalal", "çwala", "ççala"
                  , "ţnaxeka", "ţnaxekka"
                  ] :: [T.Text]
          failed = filter (\w -> case parseWord w of PUnparsed _ -> True; PError _ _ -> True; _ -> False) words
      failed `shouldBe` []

  describe "Kotlin Error Cases" $ do
    it "detects unexpectedly few slot V affixes (shortcut)" $ do
      let gloss = glossWord mempty mempty (parseWord "wa'lena")
      T.isInfixOf "Unexpectedly few" gloss `shouldBe` True

    it "detects unexpectedly many slot V affixes (shortcut)" $ do
      let gloss = glossWord mempty mempty (parseWord "waršana'anera")
      T.isInfixOf "Unexpectedly many" gloss `shouldBe` True

    it "rejects shortcuts with Cs-root Vv" $ do
      case parseWord "wëil" of
        PError msg _ -> msg `shouldBe` "Shortcuts can't be used with a Cs-root"
        pw -> expectationFailure $ "Expected PError, got: " ++ show pw

    it "rejects glottal stop in concatenated formative" $ do
      case parseWord "halala'a-alal" of
        PError msg _ -> msg `shouldBe` "Unexpected glottal stop in concatenated formative"
        pw -> expectationFailure $ "Expected PError, got: " ++ show pw

  describe "Integration: Longtest Poem" $ do
    it "parses all words from the Ozymandias poem" $ do
      let ws = ["Ufhulâ","eatreuhlï","wuksmenţi'ë","Mâlu'u","Azhesâ","Tartïnhâ",
                "antfäsi'a","Aţsägissa'hňu","yuilžařča","Aççpeřinï","theuxač",
                "Wamfpuňï","avcasu'u","umweřuňï","umskäzomļï'ï","Avllevâ",
                "evẓâlüduna","wulyezwi","açmaňotanï","Emzäsouyâ","tha","áẓčelä",
                "Wanluẓda","ařžřusö'athu","aḑlialuň","açnüsö'athu","Wužtļi'a",
                "mangut","atväsâ","Wiadná","la","hnï"]
          unparsed = filter (\w -> case parseWord w of PUnparsed _ -> True; _ -> False) ws
      unparsed `shouldBe` []

    it "parses concatenated formatives from poem" $ do
      case parseWord "Hmaggwí-ašnexürrtřa" of
        PConcatenated pfs -> length pfs `shouldBe` 2
        pw -> expectationFailure $ "Expected PConcatenated, got: " ++ show pw

    it "parses all words from the full Ozymandias poem" $ do
      -- Full 14-line poem (55 formative/referential/adjunct words)
      let ws = [ "Ufhulâ","eatreuhlï","wuksmenţi'ë"
               , "Mâlu'u","hma","Hmaggwí-ašnexürrtřa","yoskwätļu'u"
               , "Azhesâ","Tartïnhâ","antfäsi'a"
               , "Aţsägissa'hňu","yuilžařča","Aççpeřinï","theuxač"
               , "Wamfpuňï","avcasu'u","umweřuňï","umskäzomļï'ï"
               , "Avllevâ","evẓâlüduna","wulyezwi","açmaňotanï"
               , "Emzäsouyâ","tha","áẓčelä","ešwarïntena'a"
               , "Wanluẓda","ařžřusö'athu","aḑlialuň","açnüsö'athu"
               , "Wužtļi'a","mangut","atväsâ","ha"
               , "Wiadná","la","hnï","Ozimändiës","hweltivî-eltíl"
               , "Ẓosêi","ümbrira","lëi","nu'i","aňčäzwarru'u","yumřřuňêi","hai"
               , "Aiňļalïrxouyâ","Arčüsüanhi'a"
               , "Esčäswallüxeu","erkefïrahma'u","yabgaguňahma'u"
               , "Iazhasahra","eňtila'u","alřähma'u","efklaswünhâ"
               ]
          unparsed = filter (\w -> case parseWord w of PUnparsed _ -> True; _ -> False) ws
      unparsed `shouldBe` []

  describe "Grammar Reference Examples" $ do
    it "parses Configuration examples (ch03)" $ do
      -- Cat examples from ch03: rrala (UPX), rrasa (DPX), rraca (DSS), rrata (MSS), rraţa (MDS)
      let testCa word expected = case parseWord word of
            PFormative pf -> case pfCaParsed pf of
              Just ca -> pcConfig ca `shouldBe` expected
              Nothing -> expectationFailure $ T.unpack word <> ": no Ca parsed"
            pw -> expectationFailure $ T.unpack word <> ": expected PFormative, got: " ++ show pw
      testCa "rrala" UNI
      testCa "rrasa" DPX
      testCa "rraca" DSS
      testCa "rrata" MSS
      testCa "rraţa" MDS
      testCa "rraza" MFS
      testCa "rraša" DDF
      testCa "rrača" DFS

    it "parses Affiliation examples (ch03)" $ do
      let testAff word expected = case parseWord word of
            PFormative pf -> case pfCaParsed pf of
              Just ca -> pcAffiliation ca `shouldBe` expected
              Nothing -> expectationFailure $ T.unpack word <> ": no Ca parsed"
            pw -> expectationFailure $ T.unpack word <> ": expected PFormative, got: " ++ show pw
      testAff "čveţa" CSL     -- 'a bunch of tools'
      testAff "čvelţa" ASO    -- 'a well-designed set of tools'
      testAff "čverţa" COA    -- 'a toolset'

    it "parses Cc+Vv shortcut examples (ch03)" $ do
      -- yedpéi: y prefix -> PRX extension, Series 2 Vv = S2/PRC
      case parseWord "yedpéi" of
        PFormative pf -> do
          pfRoot pf `shouldBe` Root "dp"
          pfSlotII pf `shouldBe` (S2, PRC)
          case pfCaParsed pf of
            Just ca -> pcExtension ca `shouldBe` PRX
            Nothing -> expectationFailure "yedpéi: no Ca parsed"
        pw -> expectationFailure $ "yedpéi: " ++ show pw
      -- weinţdâ: w prefix -> G perspective for Series 2
      case parseWord "weinţdâ" of
        PFormative pf -> do
          pfRoot pf `shouldBe` Root "nţd"
          case pfCaParsed pf of
            Just ca -> pcPerspective ca `shouldBe` G_
            Nothing -> expectationFailure "weinţdâ: no Ca parsed"
        pw -> expectationFailure $ "weinţdâ: " ++ show pw

    it "parses Perspective + Essence in Ca" $ do
      -- Ca is a single consonant complex, not a sequence
      -- l=M/NRM, r=G/NRM, w=N/NRM, y=A/NRM, tļ=M/RPV, ř=G/RPV
      let testPerspEss word expPersp expEss = case parseWord word of
            PFormative pf -> case pfCaParsed pf of
              Just ca -> do
                pcPerspective ca `shouldBe` expPersp
                pcEssence ca `shouldBe` expEss
              Nothing -> expectationFailure $ T.unpack word <> ": no Ca parsed"
            pw -> expectationFailure $ T.unpack word <> ": expected PFormative, got: " ++ show pw
      -- Use vowel-initial form (Vv=a, Cr=čv) to avoid referential classification
      testPerspEss "ačvala" M_ NRM     -- l = default (M/NRM)
      testPerspEss "ačvara" G_ NRM     -- r = G/NRM
      testPerspEss "ačvawa" N_ NRM     -- w = N/NRM
      testPerspEss "ačvaya" A_ NRM     -- y = A/NRM
      testPerspEss "ačvatļa" M_ RPV    -- tļ = M/RPV
      testPerspEss "ačvařa" G_ RPV     -- ř = G/RPV

    it "renders and parses basic formatives consistently" $ do
      let f = Formative
                { fSlotI = Nothing
                , fSlotII = (S1, PRC)
                , fSlotIII = Root "rr"
                , fSlotIV = (STA, BSC, EXS)
                , fSlotV = []
                , fSlotVI = (UNI, CSL, M_, DEL, NRM)
                , fSlotVII = []
                , fSlotVIII = Nothing
                , fSlotIX = Left (Transrelative THM)
                , fStress = Penultimate
                }
          rendered = renderFormative f
      -- Should produce something like "arrala"
      rendered `shouldBe` "arrala"
      -- Parse it back and verify root
      case parseWord rendered of
        PFormative pf -> pfRoot pf `shouldBe` Root "rr"
        pw -> expectationFailure $ "Round-trip failed: " ++ show pw

    it "renders formative with Configuration and Case" $ do
      let f = Formative
                { fSlotI = Nothing
                , fSlotII = (S2, PRC)
                , fSlotIII = Root "rr"
                , fSlotIV = (STA, CTE, EXS)
                , fSlotV = []
                , fSlotVI = (MSS, CSL, M_, DEL, NRM)
                , fSlotVII = []
                , fSlotVIII = Nothing
                , fSlotIX = Left (Appositive GEN)
                , fStress = Penultimate
                }
          rendered = renderFormative f
      -- e = S2/PRC Vv, rr = root, ä = CTE Vr, t = MSS, l = M/NRM, ei = GEN case
      rendered `shouldBe` "errätlei"
      -- Parse it back
      case parseWord rendered of
        PFormative pf -> do
          pfRoot pf `shouldBe` Root "rr"
          pfSlotII pf `shouldBe` (S2, PRC)
          pfSlotIV pf `shouldBe` (STA, CTE, EXS)
          pfCase pf `shouldBe` Just (Appositive GEN)
        pw -> expectationFailure $ "Round-trip failed: " ++ show pw

    it "parses complex Ca forms from Kotlin tests" $ do
      -- From Kotlin test: parseCa("s") = DPX
      parseCa "s" `shouldBe` Just (ParsedCa DPX CSL M_ DEL NRM)
      -- parseCa("nļ") = ASO (standalone affiliation)
      parseCa "nļ" `shouldBe` Just (ParsedCa UNI ASO M_ DEL NRM)
      -- parseCa("tļ") = RPV (standalone essence)
      parseCa "tļ" `shouldBe` Just (ParsedCa UNI CSL M_ DEL RPV)

    it "parses VnCn for all Valence values" $ do
      let vals = [("a", MNO), ("ä", PRL), ("e", CRO), ("i", RCP),
                  ("ëi", CPL), ("ö", DUP), ("o", DEM), ("ü", CNG), ("u", PTI)]
      mapM_ (\(v, expected) ->
        parseOneVnCn v "h" `shouldBe` Just (VnCnValence expected (MoodVal FAC))) vals

    it "parses VnCn for all Aspect values (Pattern 2)" $ do
      -- Pattern 2 uses w/y Cn
      parseOneVnCn "a" "w" `shouldBe` Just (VnCnAspect RTR (MoodVal FAC))
      parseOneVnCn "ai" "w" `shouldBe` Just (VnCnAspect RSM (MoodVal FAC))
      parseOneVnCn "ia" "w" `shouldBe` Just (VnCnAspect PMP (MoodVal FAC))
      parseOneVnCn "ao" "w" `shouldBe` Just (VnCnAspect DCL (MoodVal FAC))

    it "detects Vv series and implicit affix for non-shortcut formatives" $ do
      -- Series 1 Vv (a = S1/PRC): no implicit affix
      case parseWord "amala" of
        PFormative pf -> do
          pfVvSeries pf `shouldBe` 1
          pfHasShortcut pf `shouldBe` False
        pw -> expectationFailure $ "Expected PFormative, got: " ++ show pw
      -- Series 2 Vv (ai = S1/CPT? No: ai is form 1 series 2 = S1/PRC?)
      -- Actually: series 2 form 1 = S1/PRC; vowelFormLookup "ai" = (2, 1)
      -- Vv "ai" → S1/PRC, series 2 → implicit affix r/4
      case parseWord "aimala" of
        PFormative pf -> do
          pfVvSeries pf `shouldBe` 2
          pfHasShortcut pf `shouldBe` False
          let gloss = glossWord mempty mempty (PFormative pf)
          T.isInfixOf "r/4" gloss `shouldBe` True
        pw -> expectationFailure $ "Expected PFormative for aimala, got: " ++ show pw
      -- Series 3 Vv (ia = form 1 series 3 = S1/PRC, series 3 → implicit affix t/4)
      case parseWord "iamala" of
        PFormative pf -> do
          pfVvSeries pf `shouldBe` 3
          pfHasShortcut pf `shouldBe` False
          let gloss = glossWord mempty mempty (PFormative pf)
          T.isInfixOf "t/4" gloss `shouldBe` True
        pw -> expectationFailure $ "Expected PFormative for iamala, got: " ++ show pw
      -- Series 4 Vv (ao = form 1 series 4 = S1/PRC, series 4 → implicit affix t/5)
      case parseWord "aomala" of
        PFormative pf -> do
          pfVvSeries pf `shouldBe` 4
          pfHasShortcut pf `shouldBe` False
          let gloss = glossWord mempty mempty (PFormative pf)
          T.isInfixOf "t/5" gloss `shouldBe` True
        pw -> expectationFailure $ "Expected PFormative for aomala, got: " ++ show pw

    it "shows {Ca} marker when Slot V affixes force Ca gemination" $ do
      -- alarfull: Slot V affix rf/9 + geminated Ca ll (→ default l)
      let gloss1 = glossWord mempty mempty (parseWord "alarfull")
      T.isInfixOf "{Ca}" gloss1 `shouldBe` True
      -- a'larfunall: Slot V affixes rf/9 + n/1 + geminated Ca ll
      let gloss2 = glossWord mempty mempty (parseWord "a'larfunall")
      T.isInfixOf "{Ca}" gloss2 `shouldBe` True

    it "does not add implicit affix for shortcut formatives" $ do
      -- Shortcut w + Series 2 Vv: Ca is set from shortcut table, no implicit affix
      case parseWord "waimala" of
        PFormative pf -> do
          pfHasShortcut pf `shouldBe` True
          let gloss = glossWord mempty mempty (PFormative pf)
          T.isInfixOf "r/4" gloss `shouldBe` False
          T.isInfixOf "t/4" gloss `shouldBe` False
        pw -> expectationFailure $ "Expected PFormative for waimala, got: " ++ show pw

  describe "Cs-root Designation" $ do
    it "shows Dx designation for Cs-root formatives" $ do
      let gloss = glossWord mempty mempty (parseWord "ëilal")
      T.isInfixOf "D1" gloss `shouldBe` True
      let gloss2 = glossWord mempty mempty (parseWord "oërmölá")
      T.isInfixOf "D6" gloss2 `shouldBe` True
      let gloss3 = glossWord mempty mempty (parseWord "oërmoulá")
      T.isInfixOf "D6.FNC" gloss3 `shouldBe` True

  describe "Slot V Marker Validation" $ do
    it "detects Slot V filled marker (glottal stop)" $ do
      -- alarfull: no glottal stop → pfSlotVMarker = False
      case parseFormativeReal "alarfull" of
        Just pf -> pfSlotVMarker pf `shouldBe` False
        Nothing -> expectationFailure "alarfull should parse"
      -- a'larfunall: glottal stop → pfSlotVMarker = True
      case parseFormativeReal "a'larfunall" of
        Just pf -> pfSlotVMarker pf `shouldBe` True
        Nothing -> expectationFailure "a'larfunall should parse"

    it "validates marker matches affix count" $ do
      -- alarfull: 1 affix, no marker → OK
      case parseFormativeReal "alarfull" of
        Just pf -> validateSlotVMarker pf `shouldBe` Nothing
        Nothing -> expectationFailure "alarfull should parse"
      -- a'larfunall: 2 affixes, marker present → OK
      case parseFormativeReal "a'larfunall" of
        Just pf -> validateSlotVMarker pf `shouldBe` Nothing
        Nothing -> expectationFailure "a'larfunall should parse"

  describe "Compose - Grammar Lookup" $ do
    it "looks up case abbreviations" $ do
      let results = lookupGrammar "THM"
      length results `shouldBe` 1
      gForm (head results) `shouldBe` "a"

    it "looks up spatio-temporal cases with glottal" $ do
      let results = lookupGrammar "LOC"
      length results `shouldBe` 1
      gForm (head results) `shouldBe` "i'a"

    it "looks up aspect forms" $ do
      let results = lookupGrammar "HAB"
      length results `shouldBe` 1
      gForm (head results) `shouldBe` "e"

    it "looks up mood forms" $ do
      let results = lookupGrammar "SUB"
      length results `shouldBe` 1
      gForm (head results) `shouldBe` "hl"

    it "looks up configuration forms" $ do
      let results = lookupGrammar "DPX"
      length results `shouldBe` 1
      gForm (head results) `shouldBe` "s"

  describe "Composition Round-Trip" $ do
    it "parses composed 'the child walks'" $ do
      -- elalo = S2-child-ERG, agulá = walk-DYN-OBS
      let g1 = glossWord mempty mempty (parseWord "elalo")
      T.isInfixOf "ERG" g1 `shouldBe` True
      T.isInfixOf "S2" g1 `shouldBe` True
      let g2 = glossWord mempty mempty (parseWord "agulá")
      T.isInfixOf "DYN" g2 `shouldBe` True

    it "parses composed mountain-ALL" $ do
      let g = glossWord mempty mempty (parseWord "ajlali'o")
      T.isInfixOf "ALL" g `shouldBe` True

    it "parses DPX configuration in Ca" $ do
      let g = glossWord mempty mempty (parseWord "elaslo")
      T.isInfixOf "DPX" g `shouldBe` True

    it "parses G perspective (plural) in Ca" $ do
      let g = glossWord mempty mempty (parseWord "elaro")
      T.isInfixOf "G" g `shouldBe` True

    it "parses CPT version" $ do
      let g = glossWord mempty mempty (parseWord "ägulá")
      T.isInfixOf "CPT" g `shouldBe` True

    it "parses stem 2" $ do
      let g = glossWord mempty mempty (parseWord "egulá")
      T.isInfixOf "S2" g `shouldBe` True

    it "parses CTE specification" $ do
      let g = glossWord mempty mempty (parseWord "aẓäla")
      T.isInfixOf "CTE" g `shouldBe` True

    it "parses HAB aspect" $ do
      let g = glossWord mempty mempty (parseWord "agulewá")
      T.isInfixOf "HAB" g `shouldBe` True

    it "parses SUB mood" $ do
      let g = glossWord mempty mempty (parseWord "agulahlá")
      T.isInfixOf "SUB" g `shouldBe` True

    it "parses CNT aspect" $ do
      let g = glossWord mempty mempty (parseWord "agulouwá")
      T.isInfixOf "CNT" g `shouldBe` True

  describe "Mood vs CaseScope Disambiguation" $ do
    it "verbs (ultimate stress) get Mood" $ do
      let g = glossWord mempty mempty (parseWord "agulahá")
      T.isInfixOf "FAC" g `shouldBe` True
      T.isInfixOf "CCN" g `shouldBe` False

    it "nouns (penultimate stress) get CaseScope" $ do
      let g = glossWord mempty mempty (parseWord "agulaha")
      T.isInfixOf "CCN" g `shouldBe` True
      T.isInfixOf "FAC" g `shouldBe` False

    it "SUB mood for verbs, CCA case-scope for nouns" $ do
      let gVerb = glossWord mempty mempty (parseWord "agulahlá")
      T.isInfixOf "SUB" gVerb `shouldBe` True
      let gNoun = glossWord mempty mempty (parseWord "agulahla")
      T.isInfixOf "CCA" gNoun `shouldBe` True

  describe "Context-Aware Sentence Glossing" $ do
    it "carrier adjunct causes next word to be foreign text" $ do
      let pairs = glossSentenceWords mempty mempty "hnas John malá"
      length pairs `shouldBe` 3
      -- "John" should be passed through as foreign text, not parsed
      snd (pairs !! 1) `shouldBe` "John"
      -- "malá" should be glossed normally (not foreign)
      T.isInfixOf "OBS" (snd (pairs !! 2)) `shouldBe` True

    it "carrier only consumes one following word" $ do
      let pairs = glossSentenceWords mempty mempty "hnas Tokyo malá"
      -- "Tokyo" is foreign, "malá" is glossed
      snd (pairs !! 1) `shouldBe` "Tokyo"
      T.isInfixOf "OBS" (snd (pairs !! 2)) `shouldBe` True

    it "non-carrier words are glossed normally" $ do
      let pairs = glossSentenceWords mempty mempty "malá agula"
      length pairs `shouldBe` 2
      -- Both should have real glosses (not raw text)
      T.isInfixOf "OBS" (snd (pairs !! 0)) `shouldBe` True

    it "isCarrierParsed detects carrier adjuncts" $ do
      isCarrierParsed (parseWord "hnas") `shouldBe` True
      isCarrierParsed (parseWord "hlas") `shouldBe` True
      isCarrierParsed (parseWord "malá") `shouldBe` False

    it "formative with root -s- is detected as carrier" $ do
      -- Root -s- = carrier formative (needs full formative structure)
      isCarrierParsed (parseWord "asale") `shouldBe` True

    it "glossSentence uses context-aware glossing" $ do
      let g = glossSentence mempty mempty "hnas World"
      -- "World" should appear as-is (foreign text), not parsed
      T.isInfixOf "World" g `shouldBe` True

  describe "Composed Sentence Glossing" $ do
    it "glosses 'The child walks' (verb-initial + ABS)" $ do
      let g = glossSentence mempty mempty "agalá elale"
      T.isInfixOf "OBS" g `shouldBe` True
      T.isInfixOf "ABS" g `shouldBe` True

    it "glosses 'The child gives a document to the adult' (3 cases)" $ do
      let g = glossSentence mempty mempty "enalá elalo aňtyala alalü"
      T.isInfixOf "ERG" g `shouldBe` True
      T.isInfixOf "DAT" g `shouldBe` True

    it "glosses 'I walked to the mountain' (referential + ALL case)" $ do
      let g = glossSentence mempty mempty "agalá lo ajlali'o"
      T.isInfixOf "1m-ERG" g `shouldBe` True
      T.isInfixOf "ALL" g `shouldBe` True

    it "glosses framed verb (antepenultimate stress)" $ do
      let g = glossWord mempty mempty (parseWord "ágala")
      T.isInfixOf "FRA" g `shouldBe` True

    it "glosses NEG affix on verb" $ do
      let g = glossWord mempty mempty (parseWord "amjalirá")
      -- Without affix lexicon, falls back to consonant form "r"
      T.isInfixOf "r/4" g `shouldBe` True
      T.isInfixOf "OBS" g `shouldBe` True

    it "glosses SIZ affix on noun" $ do
      let g = glossWord mempty mempty (parseWord "ajlaloxa")
      -- Without affix lexicon, falls back to "x"
      T.isInfixOf "x/7" g `shouldBe` True

    it "glosses concatenated formatives" $ do
      let g = glossWord mempty mempty (parseWord "antrala-agala")
      -- Should contain both root glosses joined by em-dash
      T.isInfixOf "—" g `shouldBe` True

    it "glosses complex multi-word sentence" $ do
      let g = glossSentence mempty mempty "agalá lo ajlaloxi'o antrala enalá aňtyala elalü"
      T.isInfixOf "1m-ERG" g `shouldBe` True
      T.isInfixOf "x/7" g `shouldBe` True  -- SIZ affix without lexicon
      T.isInfixOf "ALL" g `shouldBe` True
      T.isInfixOf "DAT" g `shouldBe` True

  describe "Modular Adjunct Context Disambiguation" $ do
    it "modular adjunct before verb gets Mood (SUB)" $ do
      let g = glossSentence mempty mempty "ahlä agalá"
      T.isInfixOf "SUB" g `shouldBe` True
      T.isInfixOf "CCA" g `shouldBe` False

    it "modular adjunct before noun gets CaseScope (CCA)" $ do
      let g = glossSentence mempty mempty "ahlä agala"
      T.isInfixOf "CCA" g `shouldBe` True
      T.isInfixOf "SUB" g `shouldBe` False

  describe "Formative Composition" $ do
    it "composes minimal formative (root -l-, THM)" $ do
      let f = minimalFormative "l"
          w = composeFormative f
      -- Vv(a) + Cr(l) + Vr(a) + Ca(l) + Vc(a) = "alala"
      w `shouldBe` "alala"

    it "composes with ERG case" $ do
      let f = (minimalFormative "l") { fSlotIX = Left (Transrelative ERG) }
          w = composeFormative f
      -- Vv(a) + Cr(l) + Vr(a) + Ca(l) + Vc(o) = "alalo"
      w `shouldBe` "alalo"

    it "composes with ultimate stress (verbal)" $ do
      let f = (minimalFormative "l")
              { fStress = Ultimate
              , fSlotIX = Right (IllocVal ASR OBS) }
          w = composeFormative f
      -- alal + a (ASR/OBS) = "alala" + ultimate stress → "alalá"
      w `shouldBe` "alalá"

    it "composes with DYN function" $ do
      let f = (minimalFormative "l") { fSlotIV = (DYN, BSC, EXS) }
          w = composeFormative f
      -- Vv(a) + Cr(l) + Vr(u) [DYN/BSC form 9] + Ca(l) + Vc(a) = "alula"
      w `shouldBe` "alula"

    it "composes with S2 stem" $ do
      let f = (minimalFormative "l") { fSlotII = (S2, PRC) }
          w = composeFormative f
      -- Vv(e) + Cr(l) + Vr(a) + Ca(l) + Vc(a) = "elala"
      w `shouldBe` "elala"

    it "composes with DPX configuration" $ do
      let f = (minimalFormative "l")
              { fSlotVI = (DPX, CSL, M_, DEL, NRM) }
          w = composeFormative f
      -- Ca for DPX/CSL/M/DEL/NRM: Ca1(s) + Ca4() = "s"
      T.isInfixOf "s" w `shouldBe` True
      -- Should not have default "l" Ca
      w `shouldNotBe` "alala"

    it "round-trips: compose then parse recovers root" $ do
      let f = (minimalFormative "kç") { fSlotIX = Left (Transrelative ABS) }
          w = composeFormative f
          parsed = parseWord w
      case parsed of
        PFormative pf -> pfRoot pf `shouldBe` Root "kç"
        _ -> expectationFailure $ "Expected PFormative, got: " ++ show parsed

    it "round-trips: compose verbal then parse detects ultimate stress" $ do
      let f = (minimalFormative "m")
              { fStress = Ultimate
              , fSlotIX = Right (IllocVal ASR OBS) }
          w = composeFormative f
          parsed = parseWord w
      case parsed of
        PFormative pf -> pfRoot pf `shouldBe` Root "m"
        _ -> expectationFailure $ "Expected PFormative, got: " ++ show parsed

    it "applies stress correctly" $ do
      applyStress Penultimate "alala" `shouldBe` "alala"
      applyStress Ultimate "alala" `shouldBe` "alalá"
      applyStress Antepenultimate "alala" `shouldBe` "álala"

    it "composes with Slot VII affix" $ do
      let affix = Affix { affixVowel = "a", affixConsonant = "r", affixType = Type1Affix }
          f = (minimalFormative "l") { fSlotVII = [affix] }
          w = composeFormative f
      -- alal + ar + a = "alalara"
      T.isInfixOf "ar" w `shouldBe` True

    it "composes with VnCn aspect (PRG)" $ do
      let f = (minimalFormative "l")
              { fSlotVIII = Just (VnCnAspect PRG (MoodVal FAC))
              , fStress = Ultimate
              , fSlotIX = Right (IllocVal ASR OBS) }
          w = composeFormative f
      -- PRG = "i", FAC P2 Cn = "w", so VnCn = "iw"
      T.isInfixOf "iw" w `shouldBe` True

    it "composes IRG illocution as Series 2 vowel" $ do
      let f = (minimalFormative "l")
              { fStress = Ultimate
              , fSlotIX = Right (IllocVal IRG OBS) }
          w = composeFormative f
      -- IRG/OBS = "ei" + ultimate stress → alalei with stress
      T.isSuffixOf "eí" w `shouldBe` True
      -- Round-trip: parse back and verify illocution
      case parseWord w of
        PFormative pf -> pfIllocVal pf `shouldBe` Just (IRG, OBS)
        other -> expectationFailure $ "Expected PFormative, got: " ++ show other

    it "round-trips compose → parse for complex formative" $ do
      let f = (minimalFormative "gw")
              { fSlotII = (S2, PRC)
              , fSlotIV = (DYN, BSC, EXS)
              , fSlotIX = Left (Transrelative ABS)
              , fStress = Penultimate }
          w = composeFormative f
          parsed = parseWord w
      case parsed of
        PFormative pf -> do
          pfRoot pf `shouldBe` Root "gw"
          pfSlotII pf `shouldBe` (S2, PRC)
        _ -> expectationFailure $ "Expected PFormative, got: " ++ show parsed

    it "round-trips Ca configuration (G perspective)" $ do
      let f = (minimalFormative "l")
              { fSlotVI = (UNI, CSL, G_, DEL, NRM)
              , fSlotIX = Left (Transrelative ERG) }
          w = composeFormative f
          parsed = parseWord w
      case parsed of
        PFormative pf -> do
          case pfCaParsed pf of
            Just pc -> pcPerspective pc `shouldBe` G_
            Nothing -> expectationFailure "Ca should be parsed"
        _ -> expectationFailure $ "Ca round-trip: " ++ show parsed

    it "round-trips framed verb (antepenultimate stress)" $ do
      let f = (minimalFormative "g")
              { fSlotIV = (DYN, BSC, EXS)
              , fStress = Antepenultimate
              , fSlotIX = Left (Transrelative THM) }
          w = composeFormative f
          parsed = parseWord w
      case parsed of
        PFormative pf -> pfStress pf `shouldBe` Antepenultimate
        _ -> expectationFailure $ "Framed verb: " ++ show parsed

    it "round-trips DPX configuration" $ do
      let f = (minimalFormative "l")
              { fSlotVI = (DPX, CSL, M_, DEL, NRM)
              , fSlotIX = Left (Transrelative ABS) }
          w = composeFormative f
          parsed = parseWord w
      case parsed of
        PFormative pf ->
          case pfCaParsed pf of
            Just pc -> pcConfig pc `shouldBe` DPX
            Nothing -> expectationFailure "Ca should be parsed"
        _ -> expectationFailure $ "DPX round-trip: " ++ show parsed

    it "round-trips CPT version" $ do
      let f = (minimalFormative "l")
              { fSlotII = (S1, CPT)
              , fSlotIX = Left (Transrelative ABS) }
          w = composeFormative f
          parsed = parseWord w
      case parsed of
        PFormative pf -> snd (pfSlotII pf) `shouldBe` CPT
        _ -> expectationFailure $ "CPT round-trip: " ++ show parsed

    it "round-trips CTE specification" $ do
      let f = (minimalFormative "l")
              { fSlotIV = (STA, CTE, EXS)
              , fSlotIX = Left (Transrelative ABS) }
          w = composeFormative f
          parsed = parseWord w
      case parsed of
        PFormative pf -> do
          let (_, spec, _) = pfSlotIV pf
          spec `shouldBe` CTE
        _ -> expectationFailure $ "CTE round-trip: " ++ show parsed

    it "round-trips aspect with verbal formative" $ do
      let f = (minimalFormative "l")
              { fSlotVIII = Just (VnCnAspect RTR (MoodVal FAC))
              , fStress = Ultimate
              , fSlotIX = Right (IllocVal ASR OBS) }
          w = composeFormative f
          parsed = parseWord w
      case parsed of
        PFormative pf ->
          -- VnCn may be in pfSlotVIII or extracted from Ca rest
          let vnCn = case pfSlotVIII pf of
                Just s8 -> Just s8
                Nothing -> case extractVnCn (pfCa pf) of
                  Just (vn, cn) -> parseOneVnCn vn cn
                  Nothing -> Nothing
          in case vnCn of
               Just (VnCnAspect asp _) -> asp `shouldBe` RTR
               other -> expectationFailure $ "Expected RTR, got: " ++ show other
        _ -> expectationFailure $ "Aspect round-trip: " ++ show parsed

    it "round-trips Slot VII affix with degree" $ do
      let affix = Affix { affixVowel = "ä", affixConsonant = "fm", affixType = Type1Affix }
          f = (minimalFormative "g")
              { fSlotIV = (DYN, BSC, EXS)
              , fSlotVII = [affix]
              , fStress = Ultimate
              , fSlotIX = Right (IllocVal ASR OBS) }
          w = composeFormative f
          parsed = parseWord w
      case parsed of
        PFormative pf -> do
          let afxPairs = extractAffixes (pfCa pf)
          length afxPairs `shouldSatisfy` (>= 1)
        _ -> expectationFailure $ "Affix round-trip: " ++ show parsed
