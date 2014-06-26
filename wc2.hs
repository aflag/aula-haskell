import System.Environment
import System.Exit
import Control.Exception
import Data.List

totalLines = show . length . lines
totalWords = show . length . words
totalLetters = show . length
stats text = intercalate " " [totalLines text, totalWords text, totalLetters text]

toTry :: IO ()
toTry = do
  args <- getArgs
  let fileName = head args
  contents <- readFile fileName
  putStrLn (stats contents ++ " " ++ fileName)

main = do
  toTry `catch` ioError `catch` errorCall
 where
  errorCall :: ErrorCall -> IO ()
  errorCall _ = putStrLn "Argumento errado" >> exitFailure
  ioError :: IOException -> IO ()
  ioError _ = putStrLn "Arquivo inválido" >> exitFailure
