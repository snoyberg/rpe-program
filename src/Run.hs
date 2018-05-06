{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run (run) where

import Import
import Check
import Render
import Data.Yaml
import RIO.FilePath
import RIO.Process

run :: RIO App ()
run = do
  yaml <- (optionsInput . appOptions) <$> ask
  let html = replaceExtension yaml "html"
      pdf = replaceExtension yaml "pdf"
  program <- liftIO $ decodeFileEither yaml >>= either throwIO pure
  check program $ do
    render html program
    proc "weasyprint" [html, pdf] runProcess_
