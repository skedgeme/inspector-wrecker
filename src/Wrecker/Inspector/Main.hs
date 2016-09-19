module Wrecker.Inspector.Main where
import Wrecker.Inspector.HAR
import Wrecker.Inspector.Options
import Wrecker

main :: IO ()
main = do 
  options <- runParser
  runner <- runHar (harFilePath options)
  run (wreckerOptions options) [(harFilePath options, runner)]