module Task
(
Task
, tryParseTask
, tryParseTaskIO
, taskToString
, taskToStringIO
, tryParseDateTime
, isTaskDueToday
, isTaskOverDue
, parseDateFormat
, DateTimeString(DateTimeString)
, markTaskAsCompleted
, isTaskCompleted
) where

import Data.Char
import Data.Time.Format
import System.Locale
import Data.Time.Clock
import Text.Regex.Posix
import Data.Maybe
import Control.Monad
import System.IO

data Repeat = Once | Daily | Weekly | Monthly | Yearly deriving (Show, Eq)

--4 supported data formats- Year-Month-Day or Month-Day-Year, separated either by slashes or dashes
data DateFormat = SlashedMDY | DashedYMD | SlashedYMD | DashedMDY deriving (Show, Eq, Ord)


data Task = Task {
  name :: String,
  dueDate :: UTCTime,
  repeat :: Repeat,
  completed :: Bool
} deriving (Show, Eq)


-- haskell type safety <3
newtype TaskName = TaskName String deriving Show
newtype DateTimeString = DateTimeString String deriving Show

-- | Create Task instance. Set repeat to 'Once' by default.
createTask :: TaskName -> UTCTime -> Maybe Repeat -> Task
createTask (TaskName name) date (Just repeat) = Task name date repeat False
createTask (TaskName name) date Nothing       = Task name date Once False


--For i/o purposes-same as createTask with completed added
createTaskIO :: TaskName -> UTCTime -> Maybe Repeat -> Bool-> Task
createTaskIO (TaskName name) date (Just repeat) complete = Task name date repeat complete
createTaskIO (TaskName name) date Nothing  complete     = Task name date Once complete


markTaskAsCompleted :: Task -> Task
markTaskAsCompleted (Task name dueDate repeat completed) =
  (Task name dueDate repeat True)



--parse the type of data format
parseDateFormat :: String -> DateFormat
parseDateFormat (a:b:c:d:s)
			|dashes && yformat = DashedYMD
			|slashes &&yformat = SlashedYMD
			|dashes = DashedMDY
			|otherwise = SlashedMDY
			where slashes= '/' `elem` (a:b:c:d:s)
			      dashes= '-' `elem` (a:b:c:d:s)
			      yformat=(isDigit(a) && isDigit(b) && isDigit(c) && isDigit(d))
-- | Parse UTCTime
--
-- Examples:
--
-- >>> tryParseDateTime (DateTimeString "2011-01-01")
-- Just 2011-01-01 00:00:00 UTC
dateTimeFormat1 = "%Y-%m-%d"
dateTimeFormat2 = "%m/%d/%Y"
dateTimeFormat3 = "%m-%d-%Y"
dateTimeFormat4 = "%Y/%m/%d"
tryParseDateTime :: DateTimeString -> Maybe UTCTime
tryParseDateTime (DateTimeString s)
		| format == DashedYMD = parseTime defaultTimeLocale dateTimeFormat1 s
		| format == SlashedMDY = parseTime defaultTimeLocale dateTimeFormat2 s
		| format == DashedMDY = parseTime defaultTimeLocale dateTimeFormat3 s
		| format == SlashedYMD = parseTime defaultTimeLocale dateTimeFormat4 s
		where format = parseDateFormat( s)

tryParseRepeat :: String -> Maybe Repeat
tryParseRepeat "once" = Just Once
tryParseRepeat "daily" = Just Daily
tryParseRepeat "weekly" = Just Weekly
tryParseRepeat "monthly" = Just Monthly
tryParseRepeat "yearly" = Just Yearly
tryParseRepeat  _ = Nothing



tryParseCompleted :: String ->Maybe Bool
tryParseCompleted "uncompleted" =Just False
tryParseCompleted "completed" =Just True

-- Parse repeat to string written to file
tryRepeatToIO :: Repeat -> String
tryRepeatToIO  Once = "*once"
tryRepeatToIO  Daily = "*daily"
tryRepeatToIO  Weekly = "*weekly"
tryRepeatToIO  Monthly = "*monthly"
tryRepeatToIO  Yearly = "*yearly"



isArrayEmpty :: [a] -> Bool
isArrayEmpty a = length a == 0

-- | Extract single regexp pattern grouping from string
tryMatch :: String -> String -> Maybe String
tryMatch string pattern =
  let matches = (string =~ pattern :: [[String]]) in
      if isArrayEmpty matches then
        Nothing
      else
        Just $ (matches !! 0) !! 1

-- | Extract the substring containing a TaskName from a string
--
-- Examples:
--
-- >>> tryFindTaskName "\"taskname\" morestuff"
-- Just (TaskName "taskname")
taskNamePattern = "\\\"(.+)\\\""
tryFindTaskName :: String -> Maybe TaskName
tryFindTaskName s = fmap TaskName (tryMatch s taskNamePattern)

-- | Extract the substring containing a DateTime from a string
--
-- Examples:
--
-- >>> tryFindDateTime "stuff ^(2013-01-01) morestuff"
-- Just (DateTimeString "2013-01-01")
--
-- >>> tryFindDateTime "stuff ^(2013-01-01 12:27)"
-- Just (DateTimeString "2013-01-01 12:27")
dateTimePattern = "\\^\\((.+)\\)"
tryFindDateTime :: String -> Maybe DateTimeString
tryFindDateTime s = fmap DateTimeString (tryMatch s dateTimePattern)

-- | Extract the substring containing a Repeat substring from a string
--
-- Examples:
--
-- >>> tryFindRepeat "stuff *daily morestuff"
-- Just "daily"
--
-- >>> tryFindRepeat "stuff *monthly"
-- Just "monthly"
repeatPattern = "\\*(.[^\\s]+)(\\s|$)"
tryFindRepeat :: String -> Maybe String
tryFindRepeat s = tryMatch s repeatPattern


completePattern = "\\%(.[^\\s]+)(\\s|$)"
tryFindComplete :: String -> Maybe String
tryFindComplete s = tryMatch s completePattern

-- find DateTime in string and parse it into UTCTime
tryFindAndParseDateTime :: String -> Maybe UTCTime
tryFindAndParseDateTime s = join $ fmap tryParseDateTime (tryFindDateTime s)

-- find Repeat in string and parse it into Repeat
tryFindAndParseRepeat :: String -> Maybe Repeat
tryFindAndParseRepeat s = join $ fmap tryParseRepeat (tryFindRepeat s)

tryFindAndParseCompleted :: String -> Maybe Bool
tryFindAndParseCompleted s = join $ fmap tryParseCompleted (tryFindComplete s)
-- | Parse a Task from Remember The Milk formatted string
--
-- Examples:
--
-- >>> tryParseTask "\"Pick up the milk\" ^(2013-01-01) *daily"
-- Left (Task {name = "Pick up the milk", dueDate = 2013-01-01 00:00:00 UTC, repeat = Daily, completed = False})
--
-- >>> tryParseTask "\"Pick up the milk\" *daily"
-- Right "Task date is missing"
--
-- >>> tryParseTask "^(2013-01-01) *daily"
-- Right "Task name is missing"
--
-- >>> tryParseTask "\"Pick up the milk\" ^(2013-01-01)"
-- Left (Task {name = "Pick up the milk", dueDate = 2013-01-01 00:00:00 UTC, repeat = Once, completed = False})
tryParseTask :: String -> Either Task String
tryParseTask s =
  let maybeName = tryFindTaskName s
      maybeDate = tryFindAndParseDateTime s
      maybeRepeat =  tryFindAndParseRepeat s in
      if isNothing maybeName then
        Right "Task name is missing"
      else if isNothing maybeDate then
        Right "Task date is missing"
      else
        Left $ createTask (fromJust maybeName) (fromJust maybeDate) maybeRepeat

-- | Parse a Task from Remember The Milk formatted string from input file

tryParseTaskIO ::String -> Either Task String
tryParseTaskIO s =
  let maybeName = tryFindTaskName (s)
      maybeDate = tryFindAndParseDateTime (s)
      maybeRepeat =  tryFindAndParseRepeat (s)
      maybeCompleted =  tryFindAndParseCompleted (s) in
      if isNothing maybeName then
        Right "Task name is missing"
      else if isNothing maybeDate then
        Right "Task date is missing"
      else if isNothing maybeCompleted then
        Right "Task repeat is missing"
      else	
	Left $ (createTaskIO (fromJust maybeName) (fromJust maybeDate) maybeRepeat (fromJust maybeCompleted))
	

-- | Return a String representation of UTCTime
--
-- Examples:
--
-- >>> let dt = fromJust $ tryParseDateTime (DateTimeString "2011-01-01")
-- >>> formatDateTime "%Y.%m.%d %H:%M" dt
-- "2011.01.01 00:00"
formatDateTime :: String -> UTCTime -> String
formatDateTime fmt dateTime = formatTime defaultTimeLocale fmt dateTime

standardDateFormat :: String
standardDateFormat = "%Y-%m-%d"

-- | Return a String representation of a Task
--
-- Examples:
--
-- >>> let (Left t) = tryParseTask "\"Pick up the milk\" ^(2013-01-01) *daily"
-- >>> let ct = markTaskAsCompleted t
-- >>> taskToString t
-- "\"Pick up the milk\"    2013.01.01 00:00"
-- >>> taskToString ct
-- "\"Pick up the milk\"    2013.01.01 00:00    completed"
taskToString (Task name dueDate repeat completed) = let
  space = "    "
  quote = "\""
  base = quote ++ name ++ quote ++ space ++ formatDateTime "%Y.%m.%d %H:%M" dueDate
  in
   if completed then
     base ++ space ++ "completed"
   else
     base

-- | Return an IO String representation of a Task

taskToStringIO (Task name dueDate repeat completed) = let
  space = " "
  quote = "\""
  base = quote ++ name ++ quote ++ space ++ "^(" ++ formatDateTime "%Y-%m-%d" dueDate ++ ")" ++ space ++ (tryRepeatToIO repeat) ++space  ++ "%" ++ (completedToChar completed)
  in
    base

-- | True if Task due date is after given date
--
-- Examples:
--
-- >>> let (Left t) = tryParseTask "\"Pick up the milk\" ^(2013-01-01) *daily"
-- >>> let dt1 = fromJust $ tryParseDateTime (DateTimeString "2013-01-02")
-- >>> let dt2 = fromJust $ tryParseDateTime (DateTimeString "2013-01-01")
-- >>> isTaskOverDue dt1 t
-- True
-- >>> isTaskOverDue dt2 t
-- False
isTaskOverDue :: UTCTime -> Task -> Bool
isTaskOverDue dateTime (Task name due repeat completed) =
  diffUTCTime dateTime due > 0


-- | True if both dateTime have the same date part
--
-- Examples:
--
-- >>> let dt1 = fromJust $ tryParseDateTime (DateTimeString "2013-01-01")
-- >>> let dt2 = fromJust $ tryParseDateTime (DateTimeString "2013-01-02")
-- >>> isDatePartEqual dt1 dt1
-- True
-- >>> isDatePartEqual dt1 dt2
-- False
isDatePartEqual :: UTCTime -> UTCTime -> Bool
isDatePartEqual dt1 dt2 = let
  d1 = formatDateTime standardDateFormat dt1
  d2 = formatDateTime standardDateFormat dt2 in
  dt1 == dt2


-- | True if task is due Today
--
-- Examples:
--
-- >>> let (Left t) = tryParseTask "\"Pick up the milk\" ^(2013-01-01) *daily"
-- >>> let dt1 = fromJust $ tryParseDateTime (DateTimeString "2013-01-01")
-- >>> let dt2 = fromJust $ tryParseDateTime (DateTimeString "2012-01-02")
-- >>> let dt3 = fromJust $ tryParseDateTime (DateTimeString "2013-01-03")
-- >>> isTaskDueToday dt1 t
-- True
-- >>> isTaskDueToday dt2 t
-- False
-- >>> isTaskDueToday dt3 t
-- True
isTaskDueToday :: UTCTime -> Task -> Bool
isTaskDueToday date (Task name dueDate repeat completed) =
  isDatePartEqual dueDate date || dueDate < date


-- | Return a String representation of a Task
--
-- Examples:
--
-- >>> let (Left t) = tryParseTask "\"Pick up the milk\" ^(2013-01-01) *daily"
-- >>> let ct = markTaskAsCompleted t
-- >>> isTaskCompleted t
-- False
-- >>> isTaskCompleted ct
-- True
isTaskCompleted :: Task -> Bool
isTaskCompleted (Task name dueDate repeat completed) = completed


completedToChar :: Bool -> String
completedToChar True = "completed"
completedToChar False = "uncompleted"





