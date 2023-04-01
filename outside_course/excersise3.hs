import Data.List
import Data.Char
import System.IO

getSearchWords :: IO [String]
getSearchWords = do
  putStrLn "Specify the words to search:"
  aux
  where
   aux = do
    putStr "> "
    line <- getLine
    if line == [] then
     return []
    else do
     xs <- aux
     return $ line:xs


findStrings :: [String] -> String -> [String]
findStrings sws text = [w | w <- sws, map toLower w `elem` ws]
 where
  ws = [lws | ws <- words text, let lws = map toLower ws]


main :: IO ()
main = do
 hSetBuffering stdout NoBuffering
 sws <- getSearchWords
 putStr "File to search: "
 path <- getLine
 text <- readFile path
 let found = findStrings sws text
 let nfound = [w | w <- sws, not $ elem w found]
 mapM_ (\s -> putStrLn $ "\"" ++ s ++ "\" found") found
 mapM_ (\s -> putStrLn $ "\"" ++ s ++ "\" NOT found") nfound