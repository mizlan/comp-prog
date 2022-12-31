module BarnTreeVerifier where

import Data.Vector (Vector, (!), (//))
import Data.Vector qualified as V
import Data.List
import Control.Monad
import Util

type O = (Int, Int, Int)
type E = (Int, Int)
type G = Vector Int

applyOrder :: [E] -> G -> O -> Maybe G
applyOrder e g (f, t, a)
  | fAmt < a = Nothing
  | null $ [(f, t), (t, f)] \\ e = Nothing
  | otherwise = pure $ g // [(f, fAmt - a), (t, tAmt + a)]
 where
  fAmt = g ! f
  tAmt = g ! t

input :: FilePath -> IO (Int, Vector Int, [E])
input inputFile = do
  (n' : amts' : e') <- lines <$> readFile inputFile
  let n = read n'
      amts = V.fromList . map read . words $ amts'
      e = map (to2 . map read . words) e'
  pure (n, amts, e)

verify num = do
  (n, amts, e) <- input $ "./input/barntree/" <> num <> ".in"
  let avg = sum amts `div` n
  (p: os') <- lines <$> getContents
  when (read p /= length os') $ error "incorrect length"
  let os = to3 . map read . words <$> os'
  case foldM (applyOrder e) amts os of
       Just v -> print $ all (== avg) v
       _ -> putStrLn "illegal op"
