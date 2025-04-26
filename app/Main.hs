{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Data.Char (toUpper)
import Data.Foldable qualified as M
import Data.Function (on)
import Data.List (intersperse, nub, sort, sortBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, isJust)
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
  optional,
  progDesc,
  short,
  strOption,
 )

data Files = Files
  { ta :: !String
  , la :: Maybe String
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
    <*> optional
      ( strOption
          ( long "la"
              <> short 'l'
              <> metavar "La"
              <> help "List of allowed words"
          )
      )

data HitOrMiss = Hit Char Int | SemiHit Char Int | Miss Char deriving (Show, Eq, Ord)

generateHits :: [(Char, Char)] -> Maybe [HitOrMiss]
generateHits = (sort <$>) . generateHits' 0
 where
  generateHits' _ [] = Just []
  generateHits' n ((c, h) : chs) = case h of
    'G' -> (Hit c n :) <$> rest
    'Y' -> (SemiHit c n :) <$> rest
    'g' -> (Miss c :) <$> rest
    _ -> Nothing
   where
    rest = generateHits' (n + 1) chs

checkHits :: M.Map Char Int -> [HitOrMiss] -> T.Text -> Bool
checkHits _ [] _ = True
checkHits cnt (Hit c i : xs) w = T.index w i == c && checkHits (M.alter (maybe (pure 1) $ pure . (+ 1)) c cnt) xs (coverLetter i w)
checkHits cnt (SemiHit c i : xs) w = any (`T.elem` w) [c, toUpper c] && T.index w i /= c && checkHits (M.alter (maybe (pure 1) pure) c cnt) xs w
checkHits cnt (Miss c : xs) w =
  let num1 = fromMaybe 0 $ M.lookup c cnt
      num2 = T.count (T.singleton c) w + T.count (T.singleton . toUpper $ c) w
   in num2 <= num1 && checkHits cnt xs w

coverLetter :: Int -> T.Text -> T.Text
coverLetter i word = let (b, e) = T.splitAt i word in b <> T.singleton (toUpper . T.head $ e) <> T.tail e

mergeHits :: [HitOrMiss] -> [HitOrMiss]
mergeHits [] = []
mergeHits [x] = [x]
mergeHits (x : y : xys) = if x == y then mergeHits (y : xys) else x : mergeHits (y : xys)

calculateFrequencies :: [T.Text] -> M.Map Char Double
calculateFrequencies ts =
  let m = foldr (flip $ T.foldr $ M.alter $ maybe (pure 1) $ pure . (+ 1)) M.empty ts
      s = M.sum m
   in M.map (/ s) m

highestProbability :: Int -> [T.Text] -> [T.Text]
highestProbability mx ts = sortBy (compare `on` probability) ts
 where
  m = calculateFrequencies ts
  probability t =
    if mx <= (length . nub . T.unpack $ t)
      then -T.foldr ((+) . (m M.!)) 0 t
      else 0

hitAndMiss :: [HitOrMiss] -> [Char]
hitAndMiss [] = []
hitAndMiss hs@(Hit x _ : rest) | Miss x `elem` hs = x : hitAndMiss rest
hitAndMiss (Hit _ _ : rest) = hitAndMiss rest
hitAndMiss (_ : _) = []

pruneHits :: [Char] -> [HitOrMiss] -> [HitOrMiss]
pruneHits _ [] = []
pruneHits cs (SemiHit c _ : rest) | c `elem` cs = pruneHits cs rest
pruneHits cs (h : rest) = h : pruneHits cs rest

main :: IO ()
main = do
  args <- execParser opts
  ta_data <- T.readFile $ ta args
  when (isJust $ la args) $ putStrLn "La is not used"
  let ta_words = T.words ta_data
  let hp = highestProbability 5 ta_words
  let magic_word = head hp
  T.putStrLn magic_word
  loop ta_words [] magic_word
 where
  loop ls hs mw = do
    wp <- T.words <$> T.getLine
    if any ((/= 5) . T.length) wp
      then iter "Wrong word/pattern length"
      else
        if not (null wp)
          then do
            let hits = case wp of
                  [w, p] -> generateHits (T.zip w p)
                  [p] -> generateHits (T.zip mw p)
                  _ -> Nothing
            case hits of
              Just new_hits -> do
                let h = mergeHits . sort $ pruneHits (hitAndMiss new_hits) hs <> new_hits
                let ws = highestProbability 4 . filter (checkHits M.empty h) $ ls
                mapM_ T.putStr (intersperse ", " ws) >> putStrLn ""
                when (length ws >= 3) $ loop ws h (head ws)
              Nothing -> iter "Wrong symbols or too many words"
          else iter "Not enough words"
   where
    iter txt = putStrLn txt >> loop ls hs mw
  opts =
    info
      (helper <*> files)
      ( fullDesc
          <> progDesc "Reads a word and a pattern and prints possible words"
          <> header "Wordler - A Wordle solver"
      )
