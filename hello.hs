main = putStrLn myhtml
myhtml = wrapHtml "Hello, world"

wrapHtml content = "<html><body>" <> content <> "</body></html>"

