module Lib where

dynLoad :: String -> IO ()  
dynLoad src = do doc <- HTML5.document
                 node     <- documentCreateElement "script"
                 elementSetAttribute node "src"   src
                 elementSetAttribute node "type"  "text/javascript"
                 -- Append the tag
                 headTags <- documentGetElementsByTagName doc "head"
                 headTag  <- nodeListItem headTags 0
                 elementAppendChild headTag node