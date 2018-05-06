{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Types where

import RIO hiding (Set)
import RIO.Process
import Data.Yaml

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  , optionsInput :: !FilePath
  }

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , appOptions :: !Options
  -- Add other app-specific configuration information here
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })

data Program = Program
  { programDays :: !(Vector Day)
  , programWeekNames :: !(Vector Text)
  }
  deriving Show
instance FromJSON Program where
  parseJSON = withObject "Program" $ \o -> Program
    <$> o .: "days"
    <*> o .: "week-names"

data Day = Day
  { dayName :: !Text
  , dayExercises :: !(Vector Exercise)
  }
  deriving Show
instance FromJSON Day where
  parseJSON = withObject "Day" $ \o -> Day
    <$> o .: "name"
    <*> o .: "exercises"

data Exercise = Exercise
  { exerciseName :: !Text
  , exerciseWeeks :: !(Vector Sets)
  }
  deriving Show
instance FromJSON Exercise where
  parseJSON = withObject "Exercise" $ \o -> Exercise
    <$> o .: "name"
    <*> o .: "weeks"

newtype Sets = Sets { unSets :: Vector Set }
  deriving (Show, FromJSON)

data Set = Set
  { setReps :: !Int
  , setRpe :: !Int
  , setCount :: !(Maybe Int)
  }
  deriving Show
instance FromJSON Set where
  parseJSON = withObject "Set" $ \o -> Set
    <$> o .: "reps"
    <*> o .: "rpe"
    <*> o .:? "count"
