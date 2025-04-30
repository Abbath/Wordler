{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Data.Char (toUpper)
import Data.Foldable qualified as M
import Data.Function (on, (&))
import Data.List (foldl', group, intersperse, sort, sortBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
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
  { possible :: !String
  , allowed :: Maybe String
  }

files :: Parser Files
files =
  Files
    <$> strOption
      ( long "possible"
          <> short 'p'
          <> metavar "POSSIBLE"
          <> help "List of possible solutions"
      )
    <*> optional
      ( strOption
          ( long "allowed"
              <> short 'a'
              <> metavar "ALLOWED"
              <> help "List of allowed words"
          )
      )

data HitOrMiss = Hit Char Int | SemiHit Char Int | Miss Char deriving (Show, Eq, Ord)

generateHits :: [(Char, Char)] -> Maybe [HitOrMiss]
generateHits = fmap (sort . zipWith (&) [0 ..]) . traverse generateHit
 where
  generateHit (c, h) = case h of
    'G' -> Just $ Hit c
    'Y' -> Just $ SemiHit c
    'g' -> Just . const $ Miss c
    _ -> Nothing

checkHits :: M.Map Char Int -> [HitOrMiss] -> T.Text -> Bool
checkHits _ [] _ = True
checkHits cnt hits w = case hits of
  (Hit c i : xs) -> T.index w i == c && checkHits (M.alter (pure . maybe 1 (+ 1)) c cnt) xs (coverLetter i w)
  (SemiHit c i : xs) -> any (`T.elem` w) [c, toUpper c] && T.index w i /= c && checkHits (M.alter (pure . fromMaybe 1) c cnt) xs w
  (Miss c : xs) ->
    let num1 = fromMaybe 0 $ M.lookup c cnt
        num2 = sum . map (flip T.count w . T.singleton) $ [c, toUpper c]
     in num2 <= num1 && checkHits cnt xs w

coverLetter :: Int -> T.Text -> T.Text
coverLetter i word = let (b, e) = T.splitAt i word in b <> T.singleton (toUpper . T.head $ e) <> T.tail e

mergeHits :: [HitOrMiss] -> [HitOrMiss]
mergeHits = map head . group

calculateFrequencies :: [T.Text] -> M.Map Char Double
calculateFrequencies ts =
  let m = foldr (flip $ T.foldr $ M.alter $ pure . maybe 1 (+ 1)) mempty ts
      s = M.sum m
   in M.map (/ s) m

highestProbability :: Int -> [T.Text] -> [T.Text]
highestProbability mx ts = sortBy (compare `on` probability) ts
 where
  m = calculateFrequencies ts
  probability t = 
    let prob = -T.foldr ((+) . (m M.!)) 0 t
        len =  length (T.foldr S.insert mempty t)
    in if mx <= len
      then prob
      else prob * (fromIntegral len / 5)

hitAndMiss :: [HitOrMiss] -> [Char]
hitAndMiss hs = foldl' (\a -> maybe a (: a) . ch) [] $ [h | h@Hit{} <- hs]
 where
  ch (Hit x _) | Miss x `elem` hs = Just x
  ch _ = Nothing

pruneHits :: [Char] -> [HitOrMiss] -> [HitOrMiss]
pruneHits cs = filter ch
 where
  ch (SemiHit c _) | c `elem` cs = False
  ch _ = True

main :: IO ()
main = do
  args <- execParser opts
  possible_words <- T.words <$> T.readFile (possible args)
  allowed_words <- maybe (pure []) (fmap T.words . T.readFile) (allowed args)
  let all_words = S.toList . foldr S.insert mempty $ possible_words <> allowed_words
  let hp = highestProbability 5 all_words
  let magic_word = head hp
  T.putStrLn magic_word
  loop all_words [] magic_word
 where
  loop ls hs mw = do
    wp <- T.words <$> T.getLine
    if any ((/= 5) . T.length) wp
      then iter "Wrong word/pattern length"
      else
        if null wp
          then iter "Not enough words"
          else do
            let hits = case wp of
                  [w, p] -> generateHits (T.zip w p)
                  [p] -> generateHits (T.zip mw p)
                  _ -> Nothing
            case hits of
              Nothing -> iter "Wrong symbols or too many words"
              Just new_hits -> do
                let h = mergeHits . sort $ pruneHits (hitAndMiss new_hits) hs <> new_hits
                let ws = highestProbability 5 . filter (checkHits mempty h) $ ls
                mapM_ T.putStr (intersperse ", " ws) >> putStrLn ""
                when (length ws > 2) $ loop ws h (head ws)
   where
    iter txt = putStrLn txt >> loop ls hs mw
  opts =
    info
      (helper <*> files)
      ( fullDesc
          <> progDesc "Reads a word and a pattern and prints possible words"
          <> header "Wordler - A Wordle solver"
      )
