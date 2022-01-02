module Main where

import System.Console.Haskeline
import Text.Megaparsec ( parse )
import Data.Text qualified as T
import Parser

main :: IO ()
main = runInputT defaultSettings repl

repl :: InputT IO ()
repl = do
  inp <- getInputLine "> "
  case inp of
    Nothing -> pure ()
    Just ":q" -> pure ()
    Just input -> do
      outputStrLn .show $ parse expr "" (T.pack input)
      repl