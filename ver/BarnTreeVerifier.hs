module BarnTreeVerifier where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import Data.Vector (Vector (..), (//))
import Data.Vector qualified as V
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as MV
import Util
import Debug.Trace
import Data.Set (Set (..))
import Data.Set qualified as S

type O = (Int, Int, Int)
type E = (Int, Int)
type G = MVector Int

mvFromList n xs = V.unsafeThaw $ V.fromListN n xs

applyOrders :: Int -> [Int] -> Set E -> [O] -> Maybe (Vector Int)
applyOrders n g es os = runST $ do
  s <- mvFromList n g
  m <- runMaybeT . forM_ os $ \(pred -> f, pred -> t, a) -> do
    fAmt <- MV.unsafeRead s f
    tAmt <- MV.unsafeRead s t
    guard $ fAmt >= a
    guard $ (f + 1, t + 1) `S.member` es
    MV.unsafeModify s (- a) f
    MV.unsafeModify s (+ a) t
  case m of
    Just _ -> pure <$> V.unsafeFreeze s
    Nothing -> pure Nothing

input :: FilePath -> IO (Int, [Int], [E])
input inputFile = do
  (n' : amts' : e') <- lines <$> readFile inputFile
  let n = read n'
      amts = map read . words $ amts'
      e = map (to2 . map read . words) e'
  pure (n, amts, e)

edges :: [E] -> Set E
edges e = S.fromList $ do
  (a, b) <- e
  [(a, b), (b, a)]

verify num = do
  (n, amts, e) <- input $ "./input/barntree/" <> num <> ".in"
  let avg = sum amts `div` n
  (p : os') <- lines <$> getContents
  (p' : _) <- fmap lines . readFile $ "./input/barntree/" <> num <> ".out"
  when (read p /= length os') $ error "inconsistent length"
  when (p /= p') $ error "incorrect length"
  let os = to3 . map read . words <$> os'
  case applyOrders n amts (edges e) os of
    Just v -> print $ all (== avg) v
    _ -> putStrLn "illegal op"
