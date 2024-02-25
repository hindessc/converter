module Markup where

import Numeric.Natural
import Data.Maybe

type Document = [Structure]

data Structure
    = Heading Natural Text
    | Paragraph [Text]
    | UnorderedList [Text]
    | OrderedList [Text]
    | CodeBlock [Text]
    deriving Show

data Property
    = Bold
    | Italics
    | Link Url
    deriving Eq

data Character
    = C Char
    | BoldStart
    | BoldEnd
    | ItalicsStart
    | ItalicsEnd
    deriving Show

type Text = [Character]

newtype Url = Url String deriving Eq

parse :: String -> Document
parse = parseLines Nothing . lines

parseLines :: Maybe Structure -> [String] -> Document
parseLines context txts =
    case txts of
        -- Done case
        [] -> maybeToList context

        ('*':num:' ': line) : rest ->
            maybe id (:) context $ Heading (charToNat num) (processLine line) : parseLines Nothing rest

        -- Unordered list case
        ('-':' ':line) : rest ->
            case context of
                Just (UnorderedList list) ->
                    parseLines (Just $ UnorderedList $ list <> [processLine line]) rest
                _ ->
                    maybe id (:) context $ parseLines (Just $ UnorderedList [processLine line]) rest

        -- Ordered list case
        ('#':' ':line) : rest ->
            case context of
                Just (OrderedList list) ->
                    parseLines (Just $ OrderedList $ list <> [processLine line]) rest
                _ ->
                    maybe id (:) context $ parseLines (Just $ OrderedList [processLine line]) rest

        -- CodeBlock
        ('>':' ':line) : rest ->
            case context of
                Just (CodeBlock list) ->
                    parseLines (Just $ CodeBlock $ list <> [processLine line]) rest
                _ ->
                    maybe id (:) context $ parseLines (Just $ CodeBlock [processLine line]) rest

        -- Paragraph case
        line : rest -> 
            case context of
                Just (Paragraph list) ->
                    parseLines (Just $ Paragraph $ list <> [processLine line]) rest
                _ ->
                    maybe id (:) context $ parseLines (Just $ Paragraph [processLine line]) rest
{-
        -- Paragraph case
        currentLine : rest ->
            let
                line = processLine currentLine
            in
                if null line then
                    maybe id (:) context $ parseLines Nothing rest
                else
                    case context of
                        Just (Paragraph paragraph) ->
                            parseLines (Just $ Paragraph $ paragraph <> line) rest
                        _ ->
                            maybe id (:) context $ parseLines (Just $ Paragraph line) rest
-}
processLine :: String -> Text
processLine x = reverse text
    where (text, _) = foldl characterType ([], []) $ trim x

characterType :: (Text, [Property]) -> Char -> (Text, [Property])
characterType (acc, properties) curr =
    case (C curr : acc) of
        -- Bold case
        (C '*':C '*':rest) -> 
            if Bold `elem` properties then
                (BoldEnd : rest, filter ((/=) Bold) properties)
            else
                (BoldStart : rest, Bold:properties)
        
        -- Italics case
        (C '~':C '~':rest) ->
            if Italics `elem` properties then
                (ItalicsEnd : rest, filter ((/=) Italics) properties)
            else (ItalicsStart : rest, Italics:properties)
        
        -- Default case
        _ -> (C curr:acc, properties)
                
trim :: String -> String
trim = unwords . words

charToNat :: Char -> Natural
charToNat x = read [x]
