module HsBlog 
  ( main
  , process
  )
  where

import qualified HsBlog.Html as Html
import qualified HsBlog.Markup as Markup
import HsBlog.Convert (convert)

import System.Directory (doesFileExist)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      contents <- getContents
      putStrLn $ process "Empty Title" contents
    [input, output] -> do
      content <- readFile output
      exists <- doesFileExist output
      let writeResult = writeFile output (process input  content)
      if exists
        then whenIO confirm writeResult
        else writeResult
    _ ->
      putStrLn "Usage: runghc Main.hs [-- <input-file> <output-file>]"

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
