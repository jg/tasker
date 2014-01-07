module Tasker where
import System.IO
import Task
import Data.Maybe
import Data.List
import Data.Traversable
import Data.Either
import System.IO.Error
import Control.Exception

help :: String
help = "Enter one of the following commands:\
\q-Quit the program \n\ 
\c- Create a new task \n\
\show-Show all tasks \n\ 
\show today-Show tasks that are incomplete and whose due date is up to current day \n\ 
\?-Help  \n\
\remove completed- Remove all completed tasks \n\
\remove- Remove the task \n\
\show completed- Show all completed tasks \n\
\mark completed- Mark the task as completed \n\
\set date-Enter a current date\n\
\load tasks- Load Tasks from a file\n\
\save tasks- Save tasks to a file\n"


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
  putStrLn("Enter the task description according to one of templates below")
  putStrLn("\"Taskname\" ^(yyyy-mm-dd) [*once/daily/weekly/monthly/yearly]")
  putStrLn("\"Taskname\" ^(yyyy/mm/dd) [*once/daily/weekly/monthly/yearly]")
  putStrLn("\"Taskname\" ^(mm/dd/yyyy) [*once/daily/weekly/monthly/yearly]")
  putStrLn("\"Taskname\" ^(mm-dd-yyyy) [*once/daily/weekly/monthly/yearly]")
  putStrLn("Example: \"Pick up the milk\" ^(2013-01-01) *daily")
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

taskListStringIO :: [Task] -> String
taskListStringIO tasks =
  unlines $ map (\task -> taskToStringIO task) tasks

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

removeTaskFromList :: [Task] -> Int -> [Task]
removeTaskFromList (task:tasks) 0 = tasks
removeTaskFromList (task:tasks) index = task : removeTaskFromList tasks (index - 1)

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
      if (length taskList==0)
	  then putStrLn("The list is empty")
	  else
	   putStrLn $ taskListString taskList
      repl taskList currentDateString
    "show today" -> let
      isTaskDue = isTaskDueToday currentDate
      isTaskOverdue = isTaskOverDue currentDate
      isTaskToBeShown =
        (\t -> (isTaskDue t || isTaskOverdue t) && not (isTaskCompleted t))
      in do
      let aList=taskListString (filter isTaskToBeShown taskList)
      if(length aList==0)
	then putStrLn("The list is empty")
	else putStrLn $ aList
      repl taskList currentDateString
    "remove completed" -> do
      repl (filter (\t -> not (isTaskCompleted t)) taskList) currentDateString
    "remove" -> do
      taskId <- getTaskIdFromUser
      repl (removeTaskFromList taskList taskId) currentDateString
    "show completed" -> do
        let aList=taskListString (filter isTaskCompleted taskList)
        if(length aList==0)
	 then putStrLn("The list is empty")
	 else putStrLn $ aList  
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
      repl taskList currentDateString
    "load tasks" -> catch(
                          do 
			     putStrLn("Type in filename to load tasks")
       			     fname <- getLine
       			     handle <- openFile fname ReadMode
			     contents <- hGetContents handle
      	 		     let linesOfTasks = init (lines contents)
			     print "Loading:"	
      	 		     print linesOfTasks	
      	 	             hClose handle
      	 		     let taskList = lefts $ fmap tryParseTaskIO linesOfTasks
			     repl taskList currentDateString  
      		     )errorHandler
		     where
		     errorHandler e =
		      if isDoesNotExistError e
	 	        then do 
				putStrLn ("No such file")
				repl taskList currentDateString   
		        else do
			        putStrLn ("File error")
                                repl taskList currentDateString    
    "save tasks" -> do
      putStrLn("Type in filename to save tasks")
      fname <- getLine
      handle <- openFile fname WriteMode
      hPutStrLn handle (taskListStringIO taskList)
      hClose handle
      repl taskList currentDateString
    cmd -> do
      putStrLn("Invalid command: Type \"?\" For help")
      putStrLn(eval(cmd))
      repl taskList currentDateString


main :: IO ()
main = let
  taskStringList = [
 --   "\"Pick up the milk\" ^(2013-01-01) *once"
             ]
  taskList = lefts $ fmap tryParseTask taskStringList in
  repl taskList (DateTimeString "2013-01-01")
