module Markup 
  ( Document
  , Structure
  )
where

import Numeric.Natural
import Data.Maybe
import Data.Word

type Document = [Structure]

data Structure 
  = Heading Natural String
  | Paragraph String
  | UnorderedList [String]
  | OrderedList [String]
  | CodeBlock [String]
  deriving Show

ex1 :: Document
ex1 = [Paragraph "Hello World"]

ex2 :: Document
ex2 = [Heading 1 "Welcome", Paragraph "To this tutorial about Haskell"]

ex3 :: Document
ex3 = 
  [ Paragraph "Remember that multiple lines with no separation are grouped together to a single paragraph but list items remain separate."
  , UnorderedList ["Item 1 of a list", "Item 2 of a list"] 
  ]

ex4 :: Document
ex4 = 
  [ Heading 1 "Compiling programs with ghc"
  , Paragraph "Running ghc invokes the Glasgow Haskell Compiler (GHC), and can be used to compile Haskell modules and programs into native executables and libraries."
  , Paragraph "Create a new Haskell source file named hello.hs, and write the following code in it:"
  , CodeBlock [ "main = putStrLn \"Hello, Haskell!\""]
  , Paragraph "Now, we can compile the program by invoking ghc with the file name:" 
  , CodeBlock 
    [ "-> ghc hello.hs"
    , "[1 0f 1] Compiling Main"  
    , "Linking hello ..."
    ]
  , Paragraph "GHC created the following files:"
  , UnorderedList
    [ "hello.hi - Haskell interface file"
    , "hello.o - Object file, the output of the compiler before linking"
    , "hello (or hello.exe on Microsoft Windows) - A native runnable executable."
    ]
  , Paragraph "GHC will produce an executable when the source file satisfies both conditions:"
  , OrderedList
    [ "Defines the main function in the source file"
    , "Defines the module name to be Main, or does not have a module declaration"
    ]
  , Paragraph "Otherwise, it will only produce the .o and .hi files."
  ]

parse :: String -> Document
parse = parseLines [] . lines

parseLines :: [String] -> [String] -> Document
parseLines currentParagraph texts =
  let paragraph = Paragraph (unlines (reverse currentParagraph))
  in
    case texts of 
      [] -> [paragraph]
      currentLine : rest ->
        if trim currentLine == ""
          then paragraph : parseLines [] rest
          else parseLines (currentLine : currentParagraph) rest

trim :: String -> String
trim = unwords . words

safeHead :: [a] -> Maybe a
safeHead list =
  case list of
    [] -> Nothing
    x : _ -> Just x

exactlyTwo :: [a] -> Maybe (a, a)
exactlyTwo list =
  case list of
    [x, y] -> Just (x , y)
    _ -> Nothing

data Color
  = RGB Word8 Word8 Word8

data Brightness
  = Dark
  | Bright

data EightColor
  = Black
  | Red
  | Green
  | Yellow
  | Blue 
  | Magenta
  | Cyan
  | White

data AnsiColor
  = AnsiColor Brightness EightColor

getBluePart :: Color -> Word8
getBluePart color =
  case color of
    RGB _ _ blue -> blue


ansiColorToVGA :: AnsiColor -> Color
ansiColorToVGA ansicolor =
  case ansicolor of
    AnsiColor Dark Black -> RGB 0 0 0
    AnsiColor Bright Black -> RGB 85 85 85
    AnsiColor Dark Red -> RGB 170 0 0
    AnsiColor Bright Red -> RGB 255 85 85

isBright :: AnsiColor -> Bool
isBright ansicolor =
  case ansicolor of
    AnsiColor Bright _ -> True
    AnsiColor _ _ -> False
    
ansiToUbuntu :: AnsiColor -> Color
ansiToUbuntu ansiColor =
  case ansiColor of 
    AnsiColor brightness color ->
      case brightness of
        Dark -> 
          case color of
            Black -> RGB 0 0 0
            Red -> RGB 194 54 33
            Green -> RGB 37 188 36
            Yellow -> RGB 173 173 39
            Blue -> RGB 73 46 225
            Magenta -> RGB 211 56 211
            Cyan -> RGB 51 187 200
            White -> RGB 203 204 205

        Bright ->
          case color of
            Black -> RGB 129 131 131
            Red -> RGB 252 57 31
            Green -> RGB 49 231 34
            Yellow -> RGB 234 236 35
            Blue -> RGB 88 51 255
            Magenta -> RGB 249 53 248
            Cyan -> RGB 20 240 240
            White -> RGB 233 235 235
        

isEmpty :: [a] -> Bool
isEmpty list = 
  case listToMaybe list of
    Nothing -> False
    Just _ -> True

isEmpty' :: [a] -> Bool
isEmpty' list =
  case list of
    [] -> True
    _ : _ -> False
