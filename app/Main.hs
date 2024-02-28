{-# LANGUAGE ViewPatterns #-}

module Main where

import Convert

import System.Environment
import System.IO

data Argument
    = Input FilePath
    | Output FilePath
    deriving Show

isInput :: Argument -> Bool
isInput (Input _) = True
isInput _ = False

isOutput :: Argument -> Bool
isOutput (Output _) = True
isOutput _ = False

main :: IO ()
main = do
    args <- getArgs
    let arguments = parse args
        input = filter (isInput) arguments
        output = filter (isOutput) arguments
    origin <- 
        if not (null input) then
            let (Input file) = head input
            in openFile file ReadMode
        else
            pure stdin
            
    hGetContents origin >>= \file ->
        print $ convert HTML file

parse ::[String] -> [Argument]
parse args =
    case args of
        [] -> []

        ("--input" : file : rest) -> Input file : parse rest
        
        ("-i" : file : rest) -> Input file : parse rest
        
        ("--output" : file : rest) -> Output file : parse rest

        ("-o" : file : rest) -> Output file : parse rest

        _ -> error "Invalid Input. Usage: cabal run converter \"input\" <file> \"output\" <file>"
            
