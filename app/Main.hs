{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Main where

import Data.Char (toUpper)
import Data.List (sort)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Options.Applicative (
  Parser,
  execParser,
  fullDesc,
  header,
  help,
  helper,
  info,
  long,
  metavar,
  progDesc,
  short,
  strOption,
 )

data Files = Files
  { la :: !String
  , ta :: !String
  }

files :: Parser Files
files =
  Files
    <$> strOption
      ( long "ta"
          <> short 't'
          <> metavar "Ta"
          <> help "List of possible solutions"
      )
    <*> strOption
      ( long "la"
          <> short 'l'
          <> metavar "La"
          <> help "List of allowed words"
      )

data HitOrMiss = Hit Char Int | SemiHit Char Int | Miss Char deriving (Show, Eq, Ord)

generateHits :: T.Text -> T.Text -> [HitOrMiss]
generateHits = generateHits' 0
 where
  generateHits' _ cs hs | all T.null [cs, hs] = []
  generateHits' n (T.uncons -> Just (c,cs)) (T.uncons -> Just (h, hs)) = case h of
    'G' -> Hit c n : rest
    'Y' -> SemiHit c n : rest
    'g' -> Miss c : rest
    _ -> error "Not suitable for generation"
    where rest = generateHits' (n + 1) cs hs
  generateHits' _ _ _ = error "Not suitable for generation"

checkHits :: [HitOrMiss] -> T.Text -> Bool
checkHits [] _ = True
checkHits (Hit c i : xs) w = T.index w i == c && checkHits xs (coverLetter i w)
checkHits (SemiHit c i : xs) w = any (`T.elem` w) [c, toUpper c] && T.index w i /= c && checkHits xs w
checkHits (Miss c : xs) w = not (T.elem c w) && checkHits xs w

coverLetter :: Int -> T.Text -> T.Text
coverLetter i word = let (b, e) = T.splitAt i word in b <> T.singleton (toUpper . T.head $ e) <> T.tail e

mergeHits :: [HitOrMiss] -> [HitOrMiss]
mergeHits [] = []
mergeHits [x] = [x]
mergeHits (x : y : xys) = if x == y then mergeHits (y : xys) else x : mergeHits (y : xys)

main :: IO ()
main = do
  args <- execParser opts
  ta_data <- T.readFile $ ta args
  -- la_data <- T.readFile $ la args
  let ta_words = T.words ta_data
  -- let la_words = T.words la_data
  loop ta_words []
 where
  loop ls hs = do
    [w, p] <- T.words <$> T.getLine
    let h = mergeHits . sort $ hs <> generateHits w p
    let ws = filter (checkHits h) ls
    print ws
    print h
    if length ws < 2
    then return ()
    else loop ls h
  opts =
    info
      (helper <*> files)
      ( fullDesc
          <> progDesc "Reads a word and a pattern and prints possible words"
          <> header "Wordler - Wordle solver"
      )
