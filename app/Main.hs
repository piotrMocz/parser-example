module Main where

import qualified Parser1

main :: IO ()
main = Parser1.run "resources/testFile.fs"
