module HsBlog.Html.Internal where

import Numeric.Natural

newtype Html = Html String

newtype Structure = Structure String

type Title = String

instance Semigroup Structure where
  (<>) c1 c2 =
    Structure (getStructureString c1 <> getStructureString c2)

instance Monoid Structure where
  mempty = empty_

html_ :: Title -> Structure -> Html
html_ title content =
  Html 
    ( el "html"
      ( el "head" (el "title" (escape title))
        <> el "body" (getStructureString content)
      ) 
    )

p_ :: String -> Structure
p_ = Structure . el "p" . escape

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

h_ :: Natural -> String -> Structure
h_ n = Structure . el ("h" <> show n) . escape 

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concat . map (el "li" . getStructureString)

el :: String -> String -> String
el tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

render :: Html -> String
render html = 
  case html of
    Html str -> str

getStructureString :: Structure -> String
getStructureString struct =
  case struct of 
    Structure str -> str

escape :: String -> String
escape = 
  let 
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot:"
        '\'' -> "&#39;"
        _ -> [c]
  in 
    concat .map escapeChar

empty_ :: Structure
empty_ = Structure ""

concatStructure :: [Structure] -> Structure
concatStructure list =
  case list of
    [] -> empty_
    x : xs -> x <> concatStructure xs

mconcatStructure :: Monoid a => [a] -> a
mconcatStructure list =
  case list of
    [] -> mempty
    x : xs -> x <> mconcatStructure xs

