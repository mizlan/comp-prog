{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module BarnTree where

import Control.Monad.State
import Data.Array
import Data.Graph
import Data.IntMap qualified as M
import Data.List.Extra
import Data.Tree
import Data.Vector qualified as V
import Debug.Trace
import Text.Pretty.Simple
import Text.Printf
import Util

data N = MkN {i :: Int, overflow :: Int}
  deriving (Show, Eq, Ord)
data Order = MkOrder {from :: Int, to :: Int, amt :: Int}
  deriving (Show)

toEdge MkOrder{..} = (from, to)

cost :: Int -> Tree N -> State [Order] Int
cost c (Node MkN{..} xs) = do
  ps <- traverse (cost c) xs
  forM_ (zip (rootLabel <$> xs) ps) $ \(MkN j _, o) -> do
    when (o > 0) $ modify' (MkOrder i j o :)
    when (o < 0) $ modify' (MkOrder j i (-o) :)
  pure . sum $ (c - overflow) : ps

makeTree edges parent root@MkN{..} = Node root $ fmap (makeTree edges root) . filter (/= parent) $ edges M.! i
ordersToEdges n = accumArray (flip (:)) [] (1, n)
collect = M.fromListWith (++) . map (\MkOrder{..} -> (from, [(to, amt)]))

solve = do
  n <- read <$> getLine
  amts <- V.fromList . map read . words <$> getLine
  let g i = MkN i $ amts V.! (i - 1)
      avg = sum amts `div` n
  edges' <- map (to2 . map read . words) <$> replicateM (n - 1) getLine
  let edges = foldl' (flip $ \(a'@(g -> a), b'@(g -> b)) -> M.insertWith (++) a' [b] . M.insertWith (++) b' [a]) M.empty edges'
      t = makeTree edges (MkN -1 -1) $ g 1
      c = execState (cost avg t) []
      m = collect c
      order = topSort . ordersToEdges n $ toEdge <$> c
  print $ length c
  forM_ order $ \f -> forM_ (M.findWithDefault [] f m) $ uncurry (printf "%d %d %d\n" f)
