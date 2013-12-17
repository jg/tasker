module Tasker where
import Control.Monad.State
import System.IO
import Task
import Data.Maybe
import Data.List
import Data.Traversable
import Data.Either

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

getCurrentDateFromUser :: IO (DateTimeString)
getCurrentDateFromUser = do
  putStrLn("Enter the current date")
  showPrompt("set date")
  line <- getLine
  return (DateTimeString line)

tryCreateTaskInteraction :: IO (Maybe Task)
tryCreateTaskInteraction = do
  taskString <- getTaskDescriptionFromUser
  case tryParseTask taskString of
    Right error -> do
      reportError(error)
      return (Nothing)
    Left task -> return (Just task)

taskListString :: [Task] -> String
taskListString tasks =
  unlines $ map (\task -> taskStringWithId task tasks) tasks

-- | Prepend the task positions so that we can select by those
taskStringWithId :: Task -> [Task] -> String
taskStringWithId task tasks = let
  id = fromJust $ elemIndex task tasks in
  show id ++ ". " ++ taskToString task

markTaskAsCompletedInTaskList :: [Task] -> Int -> [Task]
markTaskAsCompletedInTaskList (task:tasks) index =
  if index == 0 then
     (markTaskAsCompleted task) : tasks
  else
    task : (markTaskAsCompletedInTaskList tasks (index - 1))


getTaskIdFromUser :: IO Int
getTaskIdFromUser = do
  putStrLn("Enter the task id: ")
  showPrompt("task id")
  int <- getLine
  return (read int :: Int)

repl :: [Task] -> DateTimeString -> IO ()
repl taskList currentDateString = let
  currentDate = fromJust $ tryParseDateTime currentDateString
  in do
  showPrompt "main"
  input <- getLine
  case input of
    "c" -> do
      maybeTask <- (tryCreateTaskInteraction :: IO (Maybe Task))
      repl (maybeToList maybeTask ++  taskList) currentDateString
    "show" -> do
      putStrLn $ taskListString taskList
      repl taskList currentDateString
    "show today" -> let
      isTaskDue = isTaskDueToday currentDate
      isTaskOverdue = isTaskOverDue currentDate
      isTaskToBeShown =
        (\t -> (isTaskDue t || isTaskOverdue t) && not (isTaskCompleted t))
      in do
      putStrLn $ taskListString (filter isTaskToBeShown taskList)
      repl taskList currentDateString
    "show completed" -> do
      putStrLn $ taskListString (filter isTaskCompleted taskList)
      repl taskList currentDateString
    "mark completed" -> do
      taskId <- getTaskIdFromUser
      repl (markTaskAsCompletedInTaskList taskList taskId) currentDateString
    "set date" -> do
      dateString <- (getCurrentDateFromUser :: IO (DateTimeString))
      putStrLn("Date set to " ++ show dateString)
      repl taskList dateString
    "q" -> do
      return ()
    "?" -> do
      putStrLn(help)
      return ()
    cmd -> do
      putStrLn(eval(cmd))
      repl taskList currentDateString


main :: IO ()
main = let
  taskStringList = [
    "\"Pick up the milk\" ^(2013-01-01) *once"
             ]
  taskList = lefts $ fmap tryParseTask taskStringList in
  repl taskList (DateTimeString "2013-01-01")
