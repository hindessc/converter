{-# LANGUAGE ExistentialQuantification #-}
module Convert where

import Numeric.Natural
import qualified Markup
import qualified Html
import qualified Console

newtype HtmlText = HtmlText String
newtype ConsoleText = ConsoleText String

data ResultType
    = HTML
    | CONSOLE

class Result a where
    heading :: Natural -> a -> String
    paragraph :: [a] -> String
    ul :: [a] -> String
    ol :: [a] -> String
    code :: [a] -> String

instance Result HtmlText where
    heading n s = Html.getString $ Html.heading_ n $ extractHtmlText s
    paragraph s = Html.getString $ Html.paragraph_ $ map extractHtmlText s  
    ul l = Html.getString $ Html.ul_ $ map extractHtmlText l 
    ol l = Html.getString $ Html.ol_ $ map extractHtmlText l
    code c = Html.getString $ Html.code_ $ map extractHtmlText c

extractHtmlText :: HtmlText -> String 
extractHtmlText (HtmlText s) = s

instance Result ConsoleText where
    heading n s = Console.heading_ n $ extractConsoleText s
    paragraph s = Console.paragraph_ $ map extractConsoleText s
    ul        l = Console.ul_ $ map extractConsoleText l
    ol        l = Console.ol_ $ map extractConsoleText l
    code      c = Console.code_ $ map extractConsoleText c

extractConsoleText :: ConsoleText -> String
extractConsoleText (ConsoleText s) = s

convert :: ResultType -> String -> String
convert option input = finish $ concatMap process document
    where 
        document = Markup.parse input
        finish =
            case option of
                HTML    -> Html.html_ "Title"
                CONSOLE -> id
        process =
            case option of
                HTML -> processHtml
                CONSOLE -> processConsole
                

processHtml :: Markup.Structure -> String
processHtml structure =
    case structure of
        Markup.Heading num content  -> heading num $ func content
        
        (Markup.Paragraph content)  -> paragraph $ map func content

        (Markup.UnorderedList list) -> ul $ map func list

        (Markup.OrderedList list)   -> ol $ map func list

        (Markup.CodeBlock codeCont) -> code $ map func codeCont

        _ -> ""
    where
        func :: Markup.Text -> HtmlText
        func = HtmlText . convertTextHtml

convertTextHtml :: Markup.Text -> String
convertTextHtml [] = ""
convertTextHtml (x:xs) =
    case x of
        Markup.C c -> c : convertTextHtml xs
        
        Markup.BoldStart -> Html.bold_ "Start" <> convertTextHtml xs
        Markup.BoldEnd -> Html.bold_ "End"<> convertTextHtml xs

        Markup.ItalicsStart -> Html.italics_ "Start" <> convertTextHtml xs
        Markup.ItalicsEnd -> Html.italics_ "End" <> convertTextHtml xs

processConsole :: Markup.Structure -> String
processConsole structure =
    case structure of
        Markup.Heading num content  -> heading num $ func content
        
        (Markup.Paragraph content)  -> paragraph $ map func content

        (Markup.UnorderedList list) -> ul $ map func list

        (Markup.OrderedList list)   -> ol $ map func list

        (Markup.CodeBlock codeCont) -> code $ map func codeCont

        _ -> ""
    where
        func :: Markup.Text -> ConsoleText
        func = ConsoleText . convertTextConsole

convertTextConsole :: Markup.Text -> String
convertTextConsole [] = ""
convertTextConsole (x:xs) =
    case x of
        Markup.C c -> c : convertTextConsole xs
        
        Markup.BoldStart -> Console.bold_ "Start" <> convertTextConsole xs
        Markup.BoldEnd -> Console.bold_ "End"<> convertTextConsole xs

        Markup.ItalicsStart -> Console.italics_ "Start" <> convertTextConsole xs
        Markup.ItalicsEnd -> Console.italics_ "End" <> convertTextConsole xs
