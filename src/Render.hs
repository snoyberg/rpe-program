{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Render (render) where

import Import
import Data.FileEmbed
import Lucid
import qualified RIO.Vector as V

data RowWeek = RowWeek
  { rwName :: !Text
  , rwDays :: !(Vector RowDay)
  }

rwSpan :: RowWeek -> Int
rwSpan = sum . fmap rdSpan . rwDays

data RowDay = RowDay
  { rdName :: !Text
  , rdExercises :: !(Vector RowExercise)
  }

rdSpan :: RowDay -> Int
rdSpan = sum . fmap reSpan . rdExercises

data RowExercise = RowExercise
  { reName :: !Text
  , reSets :: !(Vector RowSet)
  }

reSpan :: RowExercise -> Int
reSpan = length . reSets

data RowSet = RowSet
  { rsReps :: !Int
  , rsRpe :: !Int
  }

toRowWeeks :: Program -> Vector RowWeek
toRowWeeks program =
  flip V.imap (programWeekNames program) $ \weekIdx weekName ->
  RowWeek weekName $ flip V.map (programDays program) $ \day ->
  RowDay (dayName day) $ flip V.map (dayExercises day) $ \exercise ->
  RowExercise (exerciseName exercise) $
    case exerciseWeeks exercise V.!? weekIdx of
      Nothing -> error "invariant violated: mismatched weeks"
      Just (Sets sets) -> flip V.concatMap sets $ \set ->
        V.replicate (fromMaybe 1 (setCount set)) $
        RowSet (setReps set) (setRpe set)

render :: FilePath -> Program -> RIO App ()
render fp (toRowWeeks -> weeks) = liftIO $ renderToFile fp $ do
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
      forM_ weeks $ \week ->
      iforM_ (rwDays week) $ \dayIdx day ->
      iforM_ (rdExercises day) $ \exerciseIdx exercise ->
      iforM_ (reSets exercise) $ \setIdx set -> tr_ $ do
        when (dayIdx == 0 && exerciseIdx == 0 && setIdx == 0)
          $ td_ [rowspan_ $ fromInt $ rwSpan week] $ toHtml $ rwName week
        when (exerciseIdx == 0 && setIdx == 0)
          $ td_ [rowspan_ $ fromInt $ rdSpan day] $ toHtml $ rdName day
        when (setIdx == 0)
          $ td_ [rowspan_ $ fromInt $ reSpan exercise] $ toHtml $ reName exercise
        td_ [style_ "width: 1.5cm"] $ toHtml $ concat
          [ show $ rsReps set
          , " @ "
          , show $ rsRpe set
          ]
        td_ [style_ "width: 1.5cm"] mempty
        td_ [style_ "width: 1.5cm"] mempty
        td_ [style_ "width: 1.5cm"] mempty
        td_ [style_ "width: 100%"] mempty

iforM_ v f = V.imapM_ f v

fromInt :: Int -> Text
fromInt = fromString . show

    {-
render :: FilePath -> Program -> RIO App ()
render fp program = liftIO $ renderToFile fp $ do
  meta_ [charset_ "utf-8"]
  style_ $ decodeUtf8With lenientDecode $(embedFile "static/style.css")
  forM_ (zip [0..] (toList (programWeekNames program))) $ \(i, weekName) -> do
    h1_ $ toHtml weekName
    forM_ (programDays program) $ \day -> do
      h2_ $ toHtml $ dayName day
      forM_ (dayExercises day) $ \exercise -> do
        h3_ $ toHtml $ exerciseName exercise
        table_ $ do
          thead_ $ tr_ $ do
            th_ "Goal"
            th_ "Reps"
            th_ "RPE"
          tbody_ $ forM_ (exerciseWeeks exercise V.!? i) $ \sets -> forM_ (unSets sets) $ \set ->
            replicateM_ (fromMaybe 1 (setCount set)) $ tr_ $ do
              td_ $ do
                toHtml $ show $ setReps set
                " @ "
                toHtml $ show $ setRpe set
              td_ mempty
              td_ mempty
-}
