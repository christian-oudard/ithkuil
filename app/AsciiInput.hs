{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.IO (hSetBuffering, hSetEcho, hFlush, stdin, stdout, BufferMode(..))
import Control.Exception (bracket_, catch, IOException)

data InputState
  = Idle
  | VowelRun Char Int
  | ConsPending Char

main :: IO ()
main = bracket_ setup teardown run
  where
    setup = do
      hSetBuffering stdin NoBuffering
      hSetBuffering stdout NoBuffering
      hSetEcho stdin False
    teardown = do
      hSetBuffering stdin LineBuffering
      hSetEcho stdin True
    run = do
      putStrLn "Ithkuil ASCII \x2192 Unicode  (Ctrl-D to quit)"
      putStrLn "  Doubling: aa\x2192\x00E4  ee\x2192\x00EB  oo\x2192\x00F6  uu\x2192\x00FC"
      putStrLn "  Comma:    t,\x2192\x0163  d,\x2192\x1E11  l,\x2192\x013C  c,\x2192\x00E7"
      putStrLn "  Q-suffix: sq\x2192\x0161  zq\x2192\x017E  cq\x2192\x010D  nq\x2192\x0148  rq\x2192\x0159"
      putStrLn "  Special:  dz\x2192\x1E93"
      putStrLn ""
      loop Idle 0

loop :: InputState -> Int -> IO ()
loop state pw = do
  mc <- safeGetChar
  case mc of
    Nothing     -> finish state pw
    Just '\x04' -> finish state pw
    Just '\n'   -> do
      eraseBack pw
      putStr (commitState state)
      putStrLn ""
      loop Idle 0
    Just '\DEL' -> handleBS state pw
    Just '\b'   -> handleBS state pw
    Just c      -> do
      let (committed, state') = step state c
          pw' = pendingWidth state'
      eraseBack pw
      putStr committed
      showPending state'
      hFlush stdout
      loop state' pw'

finish :: InputState -> Int -> IO ()
finish state pw = do
  eraseBack pw
  putStr (commitState state)
  putStrLn ""
  hFlush stdout

safeGetChar :: IO (Maybe Char)
safeGetChar = (Just <$> getChar) `catch` \(_ :: IOException) -> return Nothing

handleBS :: InputState -> Int -> IO ()
handleBS state pw = do
  eraseBack pw
  let state' = backspace state
  showPending state'
  hFlush stdout
  loop state' (pendingWidth state')

eraseBack :: Int -> IO ()
eraseBack 0 = return ()
eraseBack n = putStr (replicate n '\b' ++ "\ESC[K")

showPending :: InputState -> IO ()
showPending Idle            = return ()
showPending (VowelRun v n)  = putStr ("\ESC[2m" ++ replicate n v ++ "\ESC[0m")
showPending (ConsPending c) = putStr ("\ESC[2m" ++ [c] ++ "\ESC[0m")

pendingWidth :: InputState -> Int
pendingWidth Idle            = 0
pendingWidth (VowelRun _ n)  = n
pendingWidth (ConsPending _) = 1

commitState :: InputState -> String
commitState Idle            = ""
commitState (VowelRun v n)  = resolveVowels v n
commitState (ConsPending c) = [c]

backspace :: InputState -> InputState
backspace Idle            = Idle
backspace (VowelRun _ 1)  = Idle
backspace (VowelRun v n)  = VowelRun v (n - 1)
backspace (ConsPending _) = Idle

-- Pure state transition
step :: InputState -> Char -> (String, InputState)
step Idle c
  | isBufferVowel c = ("", VowelRun c 1)
  | isBufferCons c  = ("", ConsPending c)
  | otherwise       = ([c], Idle)
step (VowelRun v n) c
  | c == v    = ("", VowelRun v (n + 1))
  | otherwise =
      let vowels = resolveVowels v n
          (extra, st) = step Idle c
      in (vowels ++ extra, st)
step (ConsPending p) c = case convertCons p c of
  Just converted -> ([converted], Idle)
  Nothing
    | isBufferVowel c -> ([p], VowelRun c 1)
    | isBufferCons c  -> ([p], ConsPending c)
    | otherwise       -> ([p, c], Idle)

isBufferVowel :: Char -> Bool
isBufferVowel c = c `elem` ("aeou" :: String)

isBufferCons :: Char -> Bool
isBufferCons c = c `elem` ("tdlcsznr" :: String)

convertCons :: Char -> Char -> Maybe Char
convertCons 't' ',' = Just '\x0163'  -- ţ
convertCons 'd' ',' = Just '\x1E11'  -- ḑ
convertCons 'l' ',' = Just '\x013C'  -- ļ
convertCons 'c' ',' = Just '\x00E7'  -- ç
convertCons 's' 'q' = Just '\x0161'  -- š
convertCons 'z' 'q' = Just '\x017E'  -- ž
convertCons 'c' 'q' = Just '\x010D'  -- č
convertCons 'n' 'q' = Just '\x0148'  -- ň
convertCons 'r' 'q' = Just '\x0159'  -- ř
convertCons 'd' 'z' = Just '\x1E93'  -- ẓ
convertCons _   _   = Nothing

-- Right-grouping: eee → e + ë, eeee → ë + ë
resolveVowels :: Char -> Int -> String
resolveVowels v n =
  let dv = dieresisVowel v
      (pairs, remainder) = n `divMod` 2
  in replicate remainder v ++ replicate pairs dv

dieresisVowel :: Char -> Char
dieresisVowel 'a' = '\x00E4'  -- ä
dieresisVowel 'e' = '\x00EB'  -- ë
dieresisVowel 'o' = '\x00F6'  -- ö
dieresisVowel 'u' = '\x00FC'  -- ü
dieresisVowel c   = c
