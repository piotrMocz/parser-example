{-# LANGUAGE OverloadedStrings #-}

module Parser1 where

import           Data.Text    as Text
import qualified Data.Text.IO as Text
import           System.IO    (FilePath)

run :: FilePath -> IO ()
run filePath = do
    file <- Text.readFile filePath
    Text.putStrLn  file
