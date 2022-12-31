module Main where

import BarnTreeVerifier
import System.Environment

main :: IO ()
main = do
  [n] <- getArgs
  verify n
