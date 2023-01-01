module Main where

import BarnTreeVerifier
import BarnTree2
import System.Environment

main :: IO ()
main = do
  -- solve
  [n] <- getArgs
  verify n
