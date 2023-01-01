{-# LANGUAGE TupleSections #-}

module BarnTree2 where

import Control.Monad.State
import Data.IntMap (IntMap, (!))
import Data.IntMap qualified as M
import Data.List
import Data.List.Extra
import Data.Tree
import Data.Tuple.Extra
import Debug.Trace
import System.IO
import Text.Pretty.Simple
import Text.Printf
import Util

buildTree :: Int -> IntMap (Int, [Int]) -> Int -> Int -> Tree (Int, Int)
buildTree c g p r = Node (o - c + sumOn' (fst . rootLabel) ch, r) ch
 where
  (o, e) = g ! r
  ch = buildTree c g r <$> filter (/= p) e

input = do
  n <- read <$> getLine
  amts <- map read . words <$> getLine
  e <- map (to2 . map read . words) <$> replicateM (n - 1) getLine
  pure (n, amts, e)

solve = do
  (n, amts, e) <- input
  let avg = sum amts `div` n
  hSetBuffering stdout (BlockBuffering $ Just 8192)
  let m = M.fromDistinctAscList . zip [1 .. n] $ map (,[]) amts
      g = foldl' (flip $ \(a, b) -> M.adjust (second (b :)) a . M.adjust (second (a :)) b) m e
      t = buildTree avg g 1 1
      o = orders t
  print $ length o
  mapM_ print3 o

print3 (a, b, c) = putStrLn $ show a ++ " " ++ show b ++ " " ++ show c

orders = flip execState [] . dfs

dfs :: Tree (Int, Int) -> State [(Int, Int, Int)] ()
dfs (Node (_, i) ch) = do
  forM_ (sortOn (fst . rootLabel) ch) $ \n@(Node (o', i') _) -> case compare o' 0 of
    LT -> dfs n *> modify' ((i, i', -o') :)
    EQ -> dfs n
    GT -> modify' ((i', i, o') :) *> dfs n
