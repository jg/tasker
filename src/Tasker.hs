module Tasker where
import Control.Monad.State
import System.IO
import Task
import Data.Maybe


type TaskList = [Task]

addTask :: Task -> State TaskList ()
addTask t = state $ \xs -> ((), t:xs)

filterTaskList :: (Task -> Bool) -> State TaskList ()
filterTaskList f = state $ \xs -> ((), filter f xs)


help :: String
help = "don't panic"

eval :: String -> String
eval _ = ""


showPrompt :: String -> IO ()
showPrompt s = do
  putStr("(" ++ s ++ ")" ++ " > ")

reportError :: String -> IO ()
reportError error = do
  putStrLn("Error: " ++ error)

getTaskDescriptionFromUser :: IO String
getTaskDescriptionFromUser = do
  putStrLn("Enter the task description according to template:")
  putStrLn("\"Pick up the milk\" ^(2013-01-01) *daily")
  showPrompt("create task")
  getLine

tryCreateTaskInteraction :: IO (Maybe Task)
tryCreateTaskInteraction = do
  taskString <- getTaskDescriptionFromUser
  case tryParseTask taskString of
    Right error -> do
      reportError(error)
      return (Nothing)
    Left task -> return (Just task)

taskListString :: [Task] -> String
taskListString tasks = unlines $ map taskToString tasks

repl :: [Task] -> IO ()
repl taskList = do
  showPrompt "main"
  input <- getLine
  case input of
    "c" -> do
      maybeTask <- (tryCreateTaskInteraction :: IO (Maybe Task))
      repl (maybeToList maybeTask ++  taskList)
    "ls" -> do
      putStrLn $ taskListString taskList
      repl taskList
    "q" -> do
      return ()
    "?" -> do
      putStrLn(help)
      return ()
    cmd -> do
      putStrLn(eval(cmd))
      repl taskList


main :: IO ()
main = repl []
