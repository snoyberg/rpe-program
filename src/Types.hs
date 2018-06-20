{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Types where

import RIO hiding (Set)
import RIO.Process
import Data.Yaml
import VectorLenMatched

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

data Program weeks = Program
  { programDays :: !(Vector (Day weeks))
  , programWeekNames :: !(VectorLM weeks Text)
  }
instance weeks ~ () => FromJSON (Program weeks) where
  parseJSON = withObject "Program" $ \o -> Program
    <$> o .: "days"
    <*> (unknownKey <$> o .: "week-names")

data Day weeks = Day
  { dayName :: !Text
  , dayExercises :: !(Vector (Exercise weeks))
  }
instance weeks ~ () => FromJSON (Day weeks) where
  parseJSON = withObject "Day" $ \o -> Day
    <$> o .: "name"
    <*> o .: "exercises"

data Exercise weeks = Exercise
  { exerciseName :: !Text
  , exerciseWeeks :: !(VectorLM weeks Sets)
  }
instance weeks ~ () => FromJSON (Exercise weeks) where
  parseJSON = withObject "Exercise" $ \o -> Exercise
    <$> o .: "name"
    <*> (unknownKey <$> o .: "weeks")

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
