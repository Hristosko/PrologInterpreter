module Main where

import Interpreter
import ParserAbs
import System.IO
import System.Environment

readFiles = (fmap concat) . mapM readFile

data Command = Exit | Task Goal

--
-- parses a Command
-- i.e. Exit request or Goal
--
parseCommand :: Parser Command
parseCommand = parseExit <|> parseTask
  where
    exit _ = Exit
    parseExit = exit <$> (parseString "exit")
    parseTask = Task <$> parseGoal

run prog = do
  putStr ">> "
  hFlush stdout
  com <- getLine
  case parseStream parseCommand com of
    Left err -> do putStrLn err; run prog
    Right Exit -> return ()
    Right (Task g) -> do printSolution $ findSolutions prog (renameGoal g); run prog


main = do
  args <- getArgs
  progStr <- readFiles args
  let prog = parseStreamErr parseProgram (filter (\c -> not (elem c " \t")) progStr)
  run prog
