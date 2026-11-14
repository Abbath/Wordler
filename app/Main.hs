{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (when)
import Data.Char (toLower, toUpper)
import Data.Function (on, (&))
import Data.List (group, intersperse, sort, sortBy)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
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
              <> help "List of allowed guesses (used for opening suggestion only)"
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

incCount :: Char -> M.Map Char Int -> M.Map Char Int
incCount c = M.alter (pure . maybe 1 (+ 1)) c

coverLetter :: Int -> T.Text -> T.Text
coverLetter i word =
  let (b, e) = T.splitAt i word
   in b <> T.singleton (toUpper . T.head $ e) <> T.tail e

findUncovered :: Char -> T.Text -> Maybe Int
findUncovered c w =
  listToMaybe [i | (i, x) <- zip [0 ..] (T.unpack w), x == c]

checkHits :: M.Map Char Int -> [HitOrMiss] -> T.Text -> Bool
checkHits cnt hits w = case hits of
  [] -> True
  (Hit c i : xs) ->
    T.index w i == c && checkHits (incCount c cnt) xs (coverLetter i w)
  (SemiHit c i : xs) ->
    toLower (T.index w i) /= c
      && case findUncovered c w of
        Nothing -> False
        Just j -> checkHits (incCount c cnt) xs (coverLetter j w)
  (Miss c : xs) ->
    let num1 = fromMaybe 0 $ M.lookup c cnt
        num2 = sum . map (flip T.count w . T.singleton) $ [c, toUpper c]
     in num2 <= num1 && checkHits cnt xs w

mergeHits :: [HitOrMiss] -> [HitOrMiss]
mergeHits hs = [x | Just x <- map listToMaybe $ group hs]

calculateFrequencies :: [T.Text] -> M.Map Char Double
calculateFrequencies ts =
  let m = foldr (flip $ T.foldr $ M.alter $ pure . maybe 1 (+ 1)) mempty ts
      s = sum (M.elems m)
   in M.map (/ s) m

highestProbability :: Int -> [T.Text] -> [T.Text]
highestProbability mx ts = sortBy (compare `on` probability) ts
 where
  m = calculateFrequencies ts
  probability t =
    let prob = -T.foldr ((+) . (m M.!)) 0 t
        len = length (T.foldr S.insert mempty t)
     in if mx <= len
          then prob
          else prob * (fromIntegral len / 5)

hitAndMiss :: [HitOrMiss] -> [Char]
hitAndMiss hs = foldl' (\a -> maybe a (: a) . ch) [] [h | h@Hit{} <- hs]
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
  possible_words <- dedupe . T.words <$> T.readFile (possible args)
  allowed_words <- maybe (pure []) (fmap (dedupe . T.words) . T.readFile) (allowed args)
  case possible_words of
    [] -> putStrLn "No words"
    _ -> do
      let openers = highestProbability 5 (dedupe $ possible_words <> allowed_words)
      case openers of
        [] -> putStrLn "No words"
        (magic_word : _) -> do
          T.putStrLn magic_word
          loop possible_words [] magic_word
 where
  dedupe = S.toList . S.fromList
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
                    ws = highestProbability 5 . filter (checkHits mempty h) $ ls
                case ws of
                  [] -> putStrLn "No matches"
                  (best : rest) -> do
                    mapM_ T.putStr (intersperse ", " ws) >> putStrLn ""
                    when (not $ null rest) $ loop ws h best
   where
    iter txt = putStrLn txt >> loop ls hs mw
  opts =
    info
      (helper <*> files)
      ( fullDesc
          <> progDesc
            "Reads a guess and a pattern (G=green, Y=yellow, g=gray) \
            \and prints remaining possible solutions"
          <> header "Wordler - A Wordle solver"
      )
