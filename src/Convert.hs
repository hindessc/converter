module Convert (convert) where

import qualified Markup
import qualified Html

convert :: String -> (Markup.Document, String)
convert input = (document, Html.html_ "Title" $ concatMap (Html.getString . process) document)
    where document = Markup.parse input

process :: Markup.Structure -> Html.Structure
process structure =
    case structure of
        (Markup.Heading num content) -> Html.heading_ num $ convertText content

        (Markup.Paragraph content) -> Html.paragraph_ $ map convertText content

        (Markup.UnorderedList list) -> Html.ul_ $ map convertText list

        (Markup.OrderedList list) -> Html.ol_ $ map convertText list

        (Markup.CodeBlock code) -> Html.code_ $ map convertText code

        _ -> Html.Structure ""

convertText :: Markup.Text -> String
convertText [] = ""
convertText (x:xs) =
    case x of
        Markup.C c -> c : convertText xs
        
        Markup.BoldStart -> Html.bold_ "Start" <> convertText xs
        Markup.BoldEnd -> Html.bold_ "End"<> convertText xs

        Markup.ItalicsStart -> Html.italics_ "Start" <> convertText xs
        Markup.ItalicsEnd -> Html.italics_ "End" <> convertText xs
