import System.Environment
import Data.List

totalLines = show . length . lines
totalWords = show . length . words
totalLetters = show . length
stats text = intercalate " " [totalLines text, totalWords text, totalLetters text]

main = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  putStrLn (stats contents ++ " " ++ fileName)
