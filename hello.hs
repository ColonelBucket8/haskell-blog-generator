import Html
import Markup

main :: IO ()
main = putStrLn $ render myhtml

myhtml :: Html
myhtml = html_ "My Title" (h1_ "heading" <> (p_ "Paragraph #1" <> p_ "Paragraph #2"))
