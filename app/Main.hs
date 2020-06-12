module Main where

import DataStructures
import Process

main :: IO()
main = do
  cs <- readConfiguration "assets/Config.txt"