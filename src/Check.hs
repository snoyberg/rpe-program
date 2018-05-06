{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Check (check) where

import Import
import qualified RIO.Vector as V
import Data.Monoid (All (..))
import System.Exit (exitFailure)

check :: Program -> RIO App a -> RIO App a
check program inner = do
  All good <-
    flip foldMapM (programDays program) $ \day ->
    flip foldMapM (dayExercises day) $ \exercise ->
      if V.length (exerciseWeeks exercise) == V.length (programWeekNames program)
        then return $ All True
        else do
          logError $ "Incorrect number of weeks for exercise "
                  <> display (exerciseName exercise)
                  <> " on day "
                  <> display (dayName day)
          return $ All False
  if good then inner else liftIO exitFailure

foldMapM :: (Monad m, Foldable t, Monoid w) => (a -> m w) -> t a -> m w
foldMapM f t = foldM (\w a -> (w <>) <$> f a) mempty t
