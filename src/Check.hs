{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
module Check (check) where

import Import
import qualified RIO.Vector as V
import Data.Monoid (All (..))
import System.Exit (exitFailure)
import Data.Reflection

newtype Validation e a = Validation (Either ([e] -> [e]) a)
  deriving Functor
instance Applicative (Validation e) where
  pure = Validation . Right
  Validation (Right f) <*> Validation (Right a) = Validation (Right (f a))
  Validation (Left e1) <*> Validation (Left e2) = Validation (Left (e1 . e2))
  Validation (Left e) <*> Validation (Right _) = Validation (Left e)
  Validation (Right _) <*> Validation (Left e) = Validation (Left e)

valErr :: e -> Validation e a
valErr e = Validation (Left (e:))

runValidation :: Validation e a -> Either [e] a
runValidation (Validation (Left mkList)) = Left $ mkList []
runValidation (Validation (Right x)) = Right x

check
  :: Program ()
  -> (forall weeks. Reifies weeks Int => Program weeks -> RIO App a)
  -> RIO App a
check program inner = withVectorLM (unVectorLM (programWeekNames program)) $ \weekNames ->
  case runValidation $ traverse goDay $ programDays program of
    Left errs -> do
      mapM_ logError errs
      liftIO exitFailure
    Right days -> inner Program
      { programWeekNames = weekNames
      , programDays = days
      }
  where
    goDay (Day day exercises) = Day day <$> traverse goExercise exercises
      where
        goExercise (Exercise exercise weeks) =
          case newVectorLM $ unVectorLM weeks of
            Nothing -> valErr $ "Incorrect number of weeks for exercise "
                             <> display exercise
                             <> " on day "
                             <> display day
            Just weeks' -> pure $ Exercise exercise weeks'
