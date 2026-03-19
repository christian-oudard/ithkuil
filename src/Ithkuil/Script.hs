{-# LANGUAGE OverloadedStrings #-}
-- | Ithkuil V4 Script (Writing System)
-- Morpho-phonemic writing system with 4 character types
-- Generates SVG output for rendered formatives
module Ithkuil.Script
  ( ScriptChar(..)
  , PrimaryChar(..)
  , SecondaryChar(..)
  , TertiaryChar(..)
  , QuaternaryChar(..)
  , formativeToScript
  , scriptToSvg
  , renderFormativeSvg
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Ithkuil.Grammar
import Ithkuil.Parse (ParsedFormative(..), ParsedCa(..))

--------------------------------------------------------------------------------
-- Script Character Types
--------------------------------------------------------------------------------

-- | A sequence of script characters representing one word
data ScriptChar
  = SPrimary PrimaryChar
  | SSecondary SecondaryChar
  | STertiary TertiaryChar
  | SQuaternary QuaternaryChar
  deriving (Show, Eq)

-- | Primary Character (word-initial)
-- Encodes: Vr (Specification/Function/Context), Vv (Version/Stem), Ca complex
data PrimaryChar = PrimaryChar
  { prSpec       :: Specification
  , prFunc       :: Function
  , prCtx        :: Context
  , prStem       :: Stem
  , prVersion    :: Version
  , prConfig     :: Configuration
  , prAffil      :: Affiliation
  , prPersp      :: Perspective
  , prExtension  :: Extension
  , prEssence    :: Essence
  }
  deriving (Show, Eq)

-- | Secondary Character (consonant display)
-- Encodes: Cr (root consonant) or Cs (affix consonant)
data SecondaryChar = SecondaryChar
  { scConsonant  :: Text          -- Main consonant
  , scTopExt     :: Maybe Text    -- Top extension (cluster element)
  , scBotExt     :: Maybe Text    -- Bottom extension (cluster element)
  , scIsRotated  :: Bool          -- True for Slot VII affixes
  , scAffixType  :: Maybe AffixType  -- Dot/bar diacritic for affix type
  }
  deriving (Show, Eq)

-- | Tertiary Character (aspect/modular information)
-- Encodes: Aspect if present, or Valence + Phase info
data TertiaryChar = TertiaryChar
  { tcTopVal     :: Int           -- Top value (0-8, encodes first aspect dimension)
  , tcBotVal     :: Int           -- Bottom value (0-8, encodes second dimension)
  }
  deriving (Show, Eq)

-- | Quaternary Character (case/illocution)
-- Encodes: Vc (Case) or Vk (Illocution+Validation)
data QuaternaryChar = QuaternaryChar
  { qcTopExt     :: Int           -- Top extension (case series)
  , qcBotExt     :: Int           -- Bottom extension (case form)
  , qcMoodDiac   :: Maybe Mood    -- Superposed mood diacritic
  , qcScopeDiac  :: Maybe CaseScope  -- Case-scope diacritic
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Formative to Script Conversion
--------------------------------------------------------------------------------

-- | Convert a parsed formative to a sequence of script characters
formativeToScript :: ParsedFormative -> [ScriptChar]
formativeToScript pf =
  let (stem, ver) = pfSlotII pf
      (func, spec, ctx) = pfSlotIV pf
      Root cr = pfRoot pf
      -- Ca values
      (cfg, aff, persp, ext, ess) = case pfCaParsed pf of
        Just pc -> (pcConfig pc, pcAffiliation pc, pcPerspective pc, pcExtension pc, pcEssence pc)
        Nothing -> (UNI, CSL, M_, DEL, NRM)

      -- Primary character: Vr + Vv + Ca info
      primary = SPrimary $ PrimaryChar spec func ctx stem ver cfg aff persp ext ess

      -- Secondary character: Cr (root consonant)
      crChar = SSecondary $ consonantToSecondary cr False Nothing

      -- Slot V affixes (CsVx order → Secondary characters, not rotated)
      slotVChars = map (\(cs, _vx) ->
        SSecondary $ consonantToSecondary cs False Nothing) (pfSlotV pf)

      -- Slot VII affixes (VxCs order → Secondary characters, rotated 180°)
      slotVIIChars = map (\(_vx, cs) ->
        SSecondary $ consonantToSecondary cs True Nothing) (extractSlotVII (pfCa pf))

      -- Quaternary character: Vc/Vk (case or illocution)
      quat = case pfIllocVal pf of
        Just (ill, val) -> SQuaternary $ illocValToQuaternary ill val
        Nothing -> case pfCase pf of
          Just c -> SQuaternary $ caseToQuaternary c
          Nothing -> SQuaternary $ QuaternaryChar 0 0 Nothing Nothing

  in [primary, crChar] ++ slotVChars ++ slotVIIChars ++ [quat]

-- | Extract Slot VII affix pairs from Ca rest (same as extractAffixes but local)
extractSlotVII :: [Text] -> [(Text, Text)]
extractSlotVII parts = case parts of
  (_ca:rest) -> go rest
  _ -> []
  where
    go (v:c:rest)
      | not (T.null v) && not (T.null c)
      , T.all isVowelLike v = (v, c) : go rest
    go _ = []
    isVowelLike ch = ch `elem` ("aäeëiïöoüuáéíóúâêôû'" :: String)

-- | Convert illocution + validation to quaternary character
illocValToQuaternary :: Illocution -> Validation -> QuaternaryChar
illocValToQuaternary ill val =
  let topVal = fromEnum ill
      botVal = fromEnum val
  in QuaternaryChar topVal botVal Nothing Nothing

-- | Break a consonant cluster into core + extensions for Secondary character
consonantToSecondary :: Text -> Bool -> Maybe AffixType -> SecondaryChar
consonantToSecondary cluster rotated affType =
  let chars = T.unpack cluster
  in case chars of
    []     -> SecondaryChar "" Nothing Nothing rotated affType
    [c]    -> SecondaryChar (T.singleton c) Nothing Nothing rotated affType
    [c,d]  -> SecondaryChar (T.singleton c) (Just (T.singleton d)) Nothing rotated affType
    (c:rest) ->
      let mid = init rest
          end = [last rest]
      in SecondaryChar (T.singleton c) (Just (T.pack mid)) (Just (T.pack end)) rotated affType

-- | Convert a Case to Quaternary character values
-- Uses case series (1-8) and form within series (1-9)
caseToQuaternary :: Case -> QuaternaryChar
caseToQuaternary c =
  let (series, form) = caseSeriesForm c
  in QuaternaryChar series form Nothing Nothing

-- | Get case series (0-7) and form (0-8) for quaternary character
caseSeriesForm :: Case -> (Int, Int)
caseSeriesForm (Transrelative c) = (0, fromEnum c)
caseSeriesForm (Appositive c)    = (1, fromEnum c)
caseSeriesForm (Associative c)   = (2, fromEnum c)
caseSeriesForm (Adverbial c)     = (3, fromEnum c)
caseSeriesForm (Relational c)    = (4, fromEnum c)
caseSeriesForm (Affinitive c)    = (5, fromEnum c)
caseSeriesForm (SpatioTemporal1 c) = (6, fromEnum c)
caseSeriesForm (SpatioTemporal2 c) = (7, fromEnum c)

--------------------------------------------------------------------------------
-- SVG Rendering
--------------------------------------------------------------------------------

-- | Character dimensions
charWidth, charHeight, charSpacing :: Double
charWidth = 40
charHeight = 80
charSpacing = 8

-- | Render a sequence of script characters to SVG
scriptToSvg :: [ScriptChar] -> Text
scriptToSvg chars =
  let totalWidth = fromIntegral (length chars) * (charWidth + charSpacing) + charSpacing
      totalHeight = charHeight + 40  -- padding
      header = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\""
            <> T.pack (show (ceiling totalWidth :: Int))
            <> "\" height=\"" <> T.pack (show (ceiling totalHeight :: Int))
            <> "\" viewBox=\"0 0 " <> T.pack (show (ceiling totalWidth :: Int))
            <> " " <> T.pack (show (ceiling totalHeight :: Int)) <> "\">"
      bg = "<rect width=\"100%\" height=\"100%\" fill=\"#faf8f0\"/>"
      -- Staff lines (like musical notation)
      staffLines = T.concat
        [ staffLine 30, staffLine 50, staffLine 70, staffLine 90 ]
      charSvgs = T.concat $ zipWith renderCharAt [0..] chars
      footer = "</svg>"
  in header <> bg <> staffLines <> charSvgs <> footer

staffLine :: Double -> Text
staffLine y = "<line x1=\"0\" y1=\"" <> showD y <> "\" x2=\"100%\" y2=\""
           <> showD y <> "\" stroke=\"#ddd\" stroke-width=\"0.5\"/>"

renderCharAt :: Int -> ScriptChar -> Text
renderCharAt idx sc =
  let x = charSpacing + fromIntegral idx * (charWidth + charSpacing)
      y = 20.0
  in renderScriptChar x y sc

renderScriptChar :: Double -> Double -> ScriptChar -> Text
renderScriptChar x y (SPrimary pc) = renderPrimary x y pc
renderScriptChar x y (SSecondary sc) = renderSecondary x y sc
renderScriptChar x y (STertiary tc) = renderTertiary x y tc
renderScriptChar x y (SQuaternary qc) = renderQuaternary x y qc

-- | Render Primary Character
-- Shape encodes Specification (base shape) + Function (fill) + Context (stroke)
renderPrimary :: Double -> Double -> PrimaryChar -> Text
renderPrimary x y pc =
  let -- Base shape determined by Specification
      shape = case prSpec pc of
        BSC -> renderDiamond x y charWidth charHeight
        CTE -> renderTriangle x y charWidth charHeight
        CSV -> renderHexagon x y charWidth charHeight
        OBJ -> renderRect x y charWidth charHeight
      -- Fill determined by Function
      fill = case prFunc pc of
        STA -> "#2a4858"  -- Dark blue-gray for stative
        DYN -> "#8b4513"  -- Saddle brown for dynamic
      -- Stroke determined by Context
      stroke = case prCtx pc of
        EXS -> "#000"
        FNC -> "#444"
        RPS -> "#888"
        AMG -> "#bbb"
      -- Stem indicator: small marks at top
      stemMark = renderStemMark x y (prStem pc)
      -- Version indicator: dot for CPT
      verMark = case prVersion pc of
        CPT -> "<circle cx=\"" <> showD (x + charWidth/2) <> "\" cy=\""
            <> showD (y + 5) <> "\" r=\"2\" fill=\"" <> fill <> "\"/>"
        PRC -> ""
      -- Configuration indicator: hash marks at bottom
      cfgMark = renderConfigMark x (y + charHeight) (prConfig pc)
      -- Perspective: small symbol at right
      perspMark = renderPerspMark (x + charWidth - 5) (y + charHeight/2) (prPersp pc)
  in "<g fill=\"" <> fill <> "\" stroke=\"" <> stroke <> "\" stroke-width=\"1.5\">"
     <> shape <> stemMark <> verMark <> cfgMark <> perspMark <> "</g>"

-- | Render Secondary Character (consonant)
-- Vertical bar with extensions at top/bottom representing consonant cluster
renderSecondary :: Double -> Double -> SecondaryChar -> Text
renderSecondary x y sc =
  let cx = x + charWidth / 2
      -- Main vertical stem
      stem = "<line x1=\"" <> showD cx <> "\" y1=\"" <> showD y
          <> "\" x2=\"" <> showD cx <> "\" y2=\"" <> showD (y + charHeight)
          <> "\" stroke=\"#333\" stroke-width=\"2\"/>"
      -- Core consonant label
      label = "<text x=\"" <> showD cx <> "\" y=\"" <> showD (y + charHeight/2 + 4)
           <> "\" text-anchor=\"middle\" font-size=\"10\" font-family=\"serif\" fill=\"#333\">"
           <> scConsonant sc <> "</text>"
      -- Top extension
      topExt = case scTopExt sc of
        Nothing -> ""
        Just ext -> renderExtension cx y ext True
      -- Bottom extension
      botExt = case scBotExt sc of
        Nothing -> ""
        Just ext -> renderExtension cx (y + charHeight) ext False
      -- Rotation for Slot VII affixes
      transform = if scIsRotated sc
        then " transform=\"rotate(180 " <> showD cx <> " " <> showD (y + charHeight/2) <> ")\""
        else ""
      -- Affix type diacritic
      affDiac = case scAffixType sc of
        Just Type2Affix -> "<circle cx=\"" <> showD cx <> "\" cy=\"" <> showD (y - 5)
                        <> "\" r=\"2\" fill=\"#333\"/>"  -- Dot for Type 2
        Just Type3Affix -> "<line x1=\"" <> showD (cx - 4) <> "\" y1=\"" <> showD (y - 5)
                        <> "\" x2=\"" <> showD (cx + 4) <> "\" y2=\"" <> showD (y - 5)
                        <> "\" stroke=\"#333\" stroke-width=\"1.5\"/>"  -- Bar for Type 3
        _ -> ""
  in "<g" <> transform <> ">" <> stem <> label <> topExt <> botExt <> "</g>" <> affDiac

-- | Render Tertiary Character (aspect/valence)
renderTertiary :: Double -> Double -> TertiaryChar -> Text
renderTertiary x y tc =
  let cx = x + charWidth / 2
      -- Diamond base shape
      shape = renderDiamond x y (charWidth * 0.7) (charHeight * 0.5)
      -- Top/bottom values as notches
      topNotch = "<text x=\"" <> showD cx <> "\" y=\"" <> showD (y + 15)
              <> "\" text-anchor=\"middle\" font-size=\"8\" fill=\"#666\">"
              <> T.pack (show (tcTopVal tc)) <> "</text>"
      botNotch = "<text x=\"" <> showD cx <> "\" y=\"" <> showD (y + charHeight - 10)
              <> "\" text-anchor=\"middle\" font-size=\"8\" fill=\"#666\">"
              <> T.pack (show (tcBotVal tc)) <> "</text>"
  in "<g>" <> shape <> topNotch <> botNotch <> "</g>"

-- | Render Quaternary Character (case/illocution)
renderQuaternary :: Double -> Double -> QuaternaryChar -> Text
renderQuaternary x y qc =
  let cx = x + charWidth / 2
      -- Base: vertical bar
      bar = "<line x1=\"" <> showD cx <> "\" y1=\"" <> showD (y + 10)
         <> "\" x2=\"" <> showD cx <> "\" y2=\"" <> showD (y + charHeight - 10)
         <> "\" stroke=\"#333\" stroke-width=\"2\"/>"
      -- Top extension encodes case series (0-7)
      topExt = renderCaseExtension cx (y + 10) (qcTopExt qc) True
      -- Bottom extension encodes case form (0-8)
      botExt = renderCaseExtension cx (y + charHeight - 10) (qcBotExt qc) False
      -- Mood diacritic (superposed)
      moodDiac = case qcMoodDiac qc of
        Just mood -> renderMoodDiac cx (y + 2) mood
        Nothing -> ""
  in "<g>" <> bar <> topExt <> botExt <> moodDiac <> "</g>"

--------------------------------------------------------------------------------
-- SVG Shape Helpers
--------------------------------------------------------------------------------

renderDiamond :: Double -> Double -> Double -> Double -> Text
renderDiamond x y w h =
  let cx = x + w/2
      cy = y + h/2
      points = T.intercalate " "
        [ showD cx <> "," <> showD y
        , showD (x + w) <> "," <> showD cy
        , showD cx <> "," <> showD (y + h)
        , showD x <> "," <> showD cy
        ]
  in "<polygon points=\"" <> points <> "\"/>"

renderTriangle :: Double -> Double -> Double -> Double -> Text
renderTriangle x y w h =
  let points = T.intercalate " "
        [ showD (x + w/2) <> "," <> showD y
        , showD (x + w) <> "," <> showD (y + h)
        , showD x <> "," <> showD (y + h)
        ]
  in "<polygon points=\"" <> points <> "\"/>"

renderHexagon :: Double -> Double -> Double -> Double -> Text
renderHexagon x y w h =
  let points = T.intercalate " "
        [ showD (x + w*0.25) <> "," <> showD y
        , showD (x + w*0.75) <> "," <> showD y
        , showD (x + w) <> "," <> showD (y + h/2)
        , showD (x + w*0.75) <> "," <> showD (y + h)
        , showD (x + w*0.25) <> "," <> showD (y + h)
        , showD x <> "," <> showD (y + h/2)
        ]
  in "<polygon points=\"" <> points <> "\"/>"

renderRect :: Double -> Double -> Double -> Double -> Text
renderRect x y w h =
  "<rect x=\"" <> showD x <> "\" y=\"" <> showD y
  <> "\" width=\"" <> showD w <> "\" height=\"" <> showD h <> "\"/>"

renderExtension :: Double -> Double -> Text -> Bool -> Text
renderExtension cx y ext isTop =
  let dir = if isTop then (-1) else 1
      ey = y + dir * 15
  in "<line x1=\"" <> showD cx <> "\" y1=\"" <> showD y
  <> "\" x2=\"" <> showD (cx + 8) <> "\" y2=\"" <> showD ey
  <> "\" stroke=\"#333\" stroke-width=\"1.5\"/>"
  <> "<text x=\"" <> showD (cx + 10) <> "\" y=\"" <> showD (y + dir * 8)
  <> "\" font-size=\"7\" fill=\"#666\">" <> ext <> "</text>"

renderStemMark :: Double -> Double -> Stem -> Text
renderStemMark x y stem =
  let n :: Int
      n = case stem of { S0 -> 0; S1 -> 1; S2 -> 2; S3 -> 3 }
      marks = T.concat [ "<line x1=\"" <> showD (x + 5 + fromIntegral i * 6)
                       <> "\" y1=\"" <> showD (y - 3)
                       <> "\" x2=\"" <> showD (x + 5 + fromIntegral i * 6)
                       <> "\" y2=\"" <> showD (y + 3)
                       <> "\" stroke=\"currentColor\" stroke-width=\"1\"/>"
                       | i <- [0..n-1] ]
  in marks

renderConfigMark :: Double -> Double -> Configuration -> Text
renderConfigMark x y cfg
  | cfg == UNI = ""
  | otherwise =
    let label = T.pack $ take 3 $ show cfg
    in "<text x=\"" <> showD (x + 5) <> "\" y=\"" <> showD (y + 10)
    <> "\" font-size=\"6\" fill=\"#999\">" <> label <> "</text>"

renderPerspMark :: Double -> Double -> Perspective -> Text
renderPerspMark _ _ M_ = ""  -- Default, no mark
renderPerspMark x y G_ = "<circle cx=\"" <> showD x <> "\" cy=\"" <> showD y
                       <> "\" r=\"2\" fill=\"currentColor\"/>"
renderPerspMark x y N_ = "<line x1=\"" <> showD (x-3) <> "\" y1=\"" <> showD y
                       <> "\" x2=\"" <> showD (x+3) <> "\" y2=\"" <> showD y
                       <> "\" stroke=\"currentColor\" stroke-width=\"1\"/>"
renderPerspMark x y A_ = "<line x1=\"" <> showD (x-2) <> "\" y1=\"" <> showD (y-2)
                       <> "\" x2=\"" <> showD (x+2) <> "\" y2=\"" <> showD (y+2)
                       <> "\" stroke=\"currentColor\" stroke-width=\"1\"/>"
                       <> "<line x1=\"" <> showD (x+2) <> "\" y1=\"" <> showD (y-2)
                       <> "\" x2=\"" <> showD (x-2) <> "\" y2=\"" <> showD (y+2)
                       <> "\" stroke=\"currentColor\" stroke-width=\"1\"/>"

renderCaseExtension :: Double -> Double -> Int -> Bool -> Text
renderCaseExtension cx y val isTop =
  let dir = if isTop then (-1) else 1
      -- Different extensions based on value (0-8)
      angle = fromIntegral val * 20 - 80  -- -80 to 80 degrees
      len = 12
      ex = cx + len * sin (angle * pi / 180)
      ey = y + dir * len * cos (angle * pi / 180)
  in if val == 0 then "" else
     "<line x1=\"" <> showD cx <> "\" y1=\"" <> showD y
     <> "\" x2=\"" <> showD ex <> "\" y2=\"" <> showD ey
     <> "\" stroke=\"#333\" stroke-width=\"1.5\"/>"

renderMoodDiac :: Double -> Double -> Mood -> Text
renderMoodDiac _ _ FAC = ""
renderMoodDiac cx y SUB = "<circle cx=\"" <> showD cx <> "\" cy=\"" <> showD y
                       <> "\" r=\"2\" fill=\"#333\"/>"
renderMoodDiac cx y ASM = "<line x1=\"" <> showD (cx-3) <> "\" y1=\"" <> showD y
                       <> "\" x2=\"" <> showD (cx+3) <> "\" y2=\"" <> showD y
                       <> "\" stroke=\"#333\" stroke-width=\"1\"/>"
renderMoodDiac cx y SPC = renderMoodDiac cx y SUB <> renderMoodDiac cx (y - 5) SUB
renderMoodDiac cx y COU = "<line x1=\"" <> showD (cx-3) <> "\" y1=\"" <> showD (y-1)
                       <> "\" x2=\"" <> showD (cx+3) <> "\" y2=\"" <> showD (y-1)
                       <> "\" stroke=\"#333\" stroke-width=\"1\"/>"
                       <> "<line x1=\"" <> showD (cx-3) <> "\" y1=\"" <> showD (y+1)
                       <> "\" x2=\"" <> showD (cx+3) <> "\" y2=\"" <> showD (y+1)
                       <> "\" stroke=\"#333\" stroke-width=\"1\"/>"
renderMoodDiac cx y HYP = "<path d=\"M" <> showD (cx-3) <> " " <> showD y
                        <> " Q " <> showD cx <> " " <> showD (y-4)
                        <> " " <> showD (cx+3) <> " " <> showD y
                        <> "\" fill=\"none\" stroke=\"#333\" stroke-width=\"1\"/>"

showD :: Double -> Text
showD d = T.pack $ show (round (d * 10) `div` 10 :: Int)

--------------------------------------------------------------------------------
-- High-level API
--------------------------------------------------------------------------------

-- | Render a parsed formative directly to SVG
renderFormativeSvg :: ParsedFormative -> Text
renderFormativeSvg = scriptToSvg . formativeToScript
