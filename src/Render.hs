{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Render (render) where

import Import
import Data.FileEmbed
import Lucid
import qualified RIO.Vector as V
import Data.Reflection
import Check

weekSpan :: LMKey weeks -> Program weeks -> Int
weekSpan weekIdx = sum . fmap (daySpan weekIdx) . programDays

daySpan :: LMKey weeks -> Day weeks -> Int
daySpan weekIdx = sum . fmap (exerciseSpan weekIdx) . dayExercises

exerciseSpan :: LMKey weeks -> Exercise weeks -> Int
exerciseSpan weekIdx e =
    let Sets sets = lookupVectorLM weekIdx $ exerciseWeeks e
     in sum $ (fromMaybe 1 . setCount) <$> sets

render :: Reifies weeks Int => FilePath -> Program weeks -> RIO App ()
render fp program = liftIO $ renderToFile fp $ do
  meta_ [charset_ "utf-8"]
  style_ $ decodeUtf8With lenientDecode $(embedFile "static/style.css")
  table_ $ do
    thead_ $ do
      tr_ $ do
        th_ "Week"
        th_ "Day"
        th_ "Exercise"
        th_ "Goal"
        th_ "Weight"
        th_ "Reps"
        th_ "RPE"
        th_ "Notes"
    tbody_ $
      forM_ lmKeys $ \weekIdx ->
      iforM_ (programDays program) $ \dayIdx day ->
      iforM_ (dayExercises day) $ \exerciseIdx exercise ->
      iterateSets (lookupVectorLM weekIdx $ exerciseWeeks exercise) $ \setIdx reps rpe -> tr_ $ do
        when (dayIdx == 0 && exerciseIdx == 0 && setIdx == 0)
          $ td_ [rowspan_ $ fromInt $ weekSpan weekIdx program]
          $ toHtml $ lookupVectorLM weekIdx $ programWeekNames program
        when (exerciseIdx == 0 && setIdx == 0)
          $ td_ [rowspan_ $ fromInt $ daySpan weekIdx day] $ toHtml $ dayName day
        when (setIdx == 0)
          $ td_ [rowspan_ $ fromInt $ exerciseSpan weekIdx exercise] $ toHtml $ exerciseName exercise
        td_ [style_ "width: 1.5cm"] $ toHtml $ concat
          [ show reps
          , " @ "
          , show rpe
          ]
        td_ [style_ "width: 1.5cm"] mempty
        td_ [style_ "width: 1.5cm"] mempty
        td_ [style_ "width: 1.5cm"] mempty
        td_ [style_ "width: 100%"] mempty

iterateSets :: Monad m => Sets -> (Int -> Int -> Int -> m ()) -> m ()
iterateSets (Sets sets) f =
  V.imapM_
  (\idx set -> f idx (setReps set) (setRpe set))
  (V.concatMap (\s -> V.replicate (fromMaybe 1 (setCount s)) s) sets)

iforM_ v f = V.imapM_ f v

fromInt :: Int -> Text
fromInt = fromString . show
