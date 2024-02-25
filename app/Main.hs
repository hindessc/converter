module Main where

import Convert

main :: IO ()
main =
    readFile "sample.txt" >>= \file ->
        print $ convert file
