module Main where

import qualified Html
import qualified Markup
import Convert (convert)

import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  print $ "Hello world"

myhtml :: Html.Html
myhtml = Html.html_ "My Title" (Html.h_ 1 "heading" <> (Html.p_ "Paragraph #1" <> Html.p_ "Paragraph #2"))

process :: Html.Title -> String -> String
process  title = Html.render . convert title . Markup.parse

confirm :: IO Bool
confirm =
  putStrLn "Are you sure? (y/n)" *>
    getLine >>= \answer ->
      case answer of
        "y" -> pure True
        "n" -> pure False
        _ ->
          putStrLn "Invalid response. use y or n" *>
            confirm

whenIO :: IO Bool -> IO () -> IO ()
whenIO cond action =
  cond >>= \result ->
    if result
      then action
      else pure ()
