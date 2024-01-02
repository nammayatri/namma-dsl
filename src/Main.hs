module Main where

import App
import EulerHS.Prelude
import qualified Prelude

main :: IO ()
main = do
  Prelude.putStrLn "NOTE: I am ny-example app!"
  runRiderApp id
