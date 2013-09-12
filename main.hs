import System.IO
import Network.HTTP
import Codec.Binary.UTF8.String as UTF8
import Control.Applicative
import Text.HTML.TagSoup

main = do 
  putStr "Enter URL: "
  hFlush stdout
  url <- getLine
  response <- hrefs url 
  writeFile "result.txt" (show response)
  
openURL :: String -> IO String
openURL url = getResponseBody =<< simpleHTTP (getRequest url)

openURLUTF8 :: String -> IO String
openURLUTF8 url = UTF8.decodeString <$> openURL url

hrefs :: String -> IO [String]
hrefs url = do
  tags <- fmap parseTags $ openURLUTF8 url
  let attrs = [head i | (TagOpen "a" i) <- tags]
  return [j | ("href", j) <- attrs]
