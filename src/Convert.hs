module Convert where

import Numeric.Natural
import qualified Markup
import qualified Html

newtype HtmlText = HtmlText String
newtype ConsoleText = ConsoleText String

data Text
    = HText HtmlText
    | CText ConsoleText

data ResultType
    = HTML
    | CONSOLE

class Result a where
    heading :: Natural -> a -> String

instance Result HtmlText where
    heading n (HtmlText s) = Html.getString $ Html.heading_ n s

convert :: ResultType -> String -> (Markup.Document, String)
convert option input = (document, Html.html_ "Title" $ concatMap (process option) document)
    where document = Markup.parse input

--process :: ResultType -> Markup.Structure -> Html.Structure
process :: ResultType -> Markup.Structure -> String
process res structure =
    case structure of
        Markup.Heading num content -> heading num $ func $ convertText content
        _ -> ""
{-
        (Markup.Heading num content) -> Html.heading_ num $ convertText content

        (Markup.Paragraph content) -> Html.paragraph_ $ map convertText content

        (Markup.UnorderedList list) -> Html.ul_ $ map convertText list

        (Markup.OrderedList list) -> Html.ol_ $ map convertText list

        (Markup.CodeBlock code) -> Html.code_ $ map convertText code

        _ -> Html.Structure ""
-}
    where
        func =
            case res of
                HTML -> HtmlText

convertText :: Markup.Text -> String
convertText [] = ""
convertText (x:xs) =
    case x of
        Markup.C c -> c : convertText xs
        
        Markup.BoldStart -> Html.bold_ "Start" <> convertText xs
        Markup.BoldEnd -> Html.bold_ "End"<> convertText xs

        Markup.ItalicsStart -> Html.italics_ "Start" <> convertText xs
        Markup.ItalicsEnd -> Html.italics_ "End" <> convertText xs
