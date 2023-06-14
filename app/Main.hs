module Main where

import Data.DyNetKat
import Data.NetKat
import Result.Dot

main :: IO ()
main = do
    putStrLn $ toGraphviz $ eval example (snd $ head example) [Packet 0 0 0 0, Packet 0 0 1 0]
