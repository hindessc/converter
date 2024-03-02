module Console where

import Numeric.Natural
import Data.List

newtype Console = Console String deriving Show

type Tag = String

type Title = String

newtype Text = Text String

finish :: Title -> String -> String
finish = const

heading_ :: Natural -> String -> String
heading_ _ s = "\x1b[4m" ++ s ++ "\x1b[0m\n"

paragraph_ :: [String] -> String
paragraph_ = line

ul_ :: [String] -> String
ul_ = line . map (" - " ++)

ol_ :: [String] -> String
ol_ = line. map (\(n, s) -> (show n) ++ ". " ++ s) . zip [1..]

code_ :: [String] -> String
code_ l = "##\n" ++ line l ++ "##\n"

bold_ :: String -> String
bold_ "Start" = "\x1b[1m"
bold_ "End" = "\x1b[0m"

italics_ :: String -> String
italics_ "Start" = "\x1b[3m"
italics_ "End" = "\x1b[0m"

line :: [String] -> String
line = (++"\n") . intercalate "\n"
