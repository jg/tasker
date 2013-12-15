module Tasker where
import Control.Monad.State
import System.IO
import Task


type TaskList = [Task]

addTask :: Task -> State TaskList ()
addTask t = state $ \xs -> ((), t:xs)

filterTaskList :: (Task -> Bool) -> State TaskList ()
filterTaskList f = state $ \xs -> ((), filter f xs)


help :: String
help = "don't panic"

eval :: String -> String
eval _ = ""


showPrompt  :: IO ()
showPrompt = do
  putStr("> ")

reportError :: String -> IO ()
reportError error = do
  putStr("Error: " ++ error)

getTaskDescriptionFromUser :: IO String
getTaskDescriptionFromUser = do
  putStrLn("Enter the task description according to template:")
  putStrLn("\"Pick up the milk\" ^(2013-01-01) *daily")
  showPrompt
  getLine

tryCreateTaskInteraction :: IO (Maybe Task)
tryCreateTaskInteraction = do
  taskString <- getTaskDescriptionFromUser
  case tryParseTask taskString of
    Right error -> do
      reportError(error)
      return (Nothing)
    Left task -> return (Just task)

repl = let taskList = [] in
  do
  showPrompt
  input <- getLine
  case input of
    "c" -> do
      maybeTask <- (tryCreateTaskInteraction :: IO (Maybe Task))
      case maybeTask of
        Nothing -> return ()
        Just task -> do
          showPrompt
          return ()
    "q" -> do
      return ()
    "?" -> do
      putStrLn(help)
      return ()
    cmd -> do
      putStrLn(eval(cmd))
      repl


main :: IO ()
main = repl
