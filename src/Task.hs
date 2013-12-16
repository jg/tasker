module Task
(
Task
, tryParseTask
, taskToString
) where

import Data.Time.Format
import System.Locale
import Data.Time.Clock
import Text.Regex.Posix
import Data.Maybe
import Control.Monad

data Repeat = Once | Daily | Weekly | Monthly | Yearly deriving (Show)

data Task = Task {
  name :: String,
  dueDate :: UTCTime,
  repeat :: Repeat,
  completed :: Bool
} deriving (Show)

-- haskell type safety <3
newtype TaskName = TaskName String deriving Show
newtype DateTimeString = DateTimeString String deriving Show

-- | Create Task instance. Set repeat to 'Once' by default.
createTask :: TaskName -> UTCTime -> Maybe Repeat -> Task
createTask (TaskName name) date (Just repeat) = Task name date repeat False
createTask (TaskName name) date Nothing       = Task name date Once False

-- | Parse UTCTime
--
-- Examples:
--
-- >>> tryParseDateTime (DateTimeString "2011-01-01")
-- Just 2011-01-01 00:00:00 UTC
dateTimeFormat1 = "%Y-%m-%d"
tryParseDateTime :: DateTimeString -> Maybe UTCTime
tryParseDateTime (DateTimeString s) = parseTime defaultTimeLocale dateTimeFormat1 s

tryParseRepeat :: String -> Maybe Repeat
tryParseRepeat "once" = Just Once
tryParseRepeat "daily" = Just Daily
tryParseRepeat "weekly" = Just Weekly
tryParseRepeat "monthly" = Just Monthly
tryParseRepeat "yearly" = Just Yearly
tryParseRepeat  _ = Nothing

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

-- find DateTime in string and parse it into UTCTime
tryFindAndParseDateTime :: String -> Maybe UTCTime
tryFindAndParseDateTime s = join $ fmap tryParseDateTime (tryFindDateTime s)

-- find Repeat in string and parse it into Repeat
tryFindAndParseRepeat :: String -> Maybe Repeat
tryFindAndParseRepeat s = join $ fmap tryParseRepeat (tryFindRepeat s)

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



-- | Return a String representation of UTCTime
--
-- Examples:
--
-- >>> let dt = fromJust $ tryParseDateTime (DateTimeString "2011-01-01")
-- >>> formatDateTime dt
-- "2011.01.01 00:00"
formatDateTime :: UTCTime -> String
formatDateTime dateTime = formatTime defaultTimeLocale "%Y.%m.%d %H:%M" dateTime

-- | Return a String representation of a Task
--
-- Examples:
--
-- >>> let (Left t) = tryParseTask "\"Pick up the milk\" ^(2013-01-01) *daily"
-- >>> taskToString t
-- "\"Pick up the milk\"    2013.01.01 00:00"
taskToString (Task name dueDate repeat completed) = "\"" ++ name ++ "\"" ++ "    " ++ formatDateTime dueDate
