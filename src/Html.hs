module Html where

import Numeric.Natural
import Data.List

newtype Html = Html String deriving Show

newtype Structure = Structure String

instance Semigroup Structure where
    (Structure a) <> (Structure b) = Structure (a <> b)

type Tag = String

type Title = String

newtype Text = Text String



html_ :: Title -> String -> String
html_ title content = addTag "html" $ (addTag "title" title) <> (addTag "body" content)

heading_ :: Natural -> String -> Structure
heading_ num = structurify ("h" <> show num)

paragraph_ :: [String] -> Structure
paragraph_ = structurify "p" . lineBreak

ul_ :: [String] -> Structure
ul_ = list_ "ul"

ol_ :: [String] -> Structure
ol_ = list_ "ol"

list_ :: Tag -> [String] -> Structure
list_ option = Structure . addTag option . concatMap (addTag "li")

code_ :: [String] -> Structure
code_ = structurify "pre" . lineBreak

structurify :: Tag -> String -> Structure
structurify tag = Structure . addTag tag

addTag :: Tag -> String -> String
addTag tag = addLeftTag tag . addRightTag tag

addLeftTag :: Tag -> String -> String
addLeftTag tag content = "<" <> tag <> ">" <> content

addRightTag :: Tag -> String -> String
addRightTag tag content = content <> "</" <> tag <> ">"

getString :: Structure -> String
getString (Structure s) = s

lineBreak :: [String] -> String
lineBreak = intercalate "<br>"

bold_ :: String -> String
bold_ "Start" = "<b>"
bold_ "End" = "</b>"

italics_ :: String -> String
italics_ "Start" = "<i>"
italics_ "End" = "</i>"
