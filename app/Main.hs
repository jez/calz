{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
import           Data.Time
import           System.Console.Docopt
import           System.Environment                        (getArgs)

import           Calz.ArgParser
import           Calz.Layout.Flow
import           Calz.Layout.Grid
import           Calz.Types

parseArgvOrThrow :: Day -> [String] -> IO (Config, DatePhrase)
parseArgvOrThrow today argv =
  either (exitWithUsageMessage patterns) return $ parseArgv today argv

main :: IO ()
main = do
  today            <- localDay . zonedTimeToLocalTime <$> getZonedTime
  (config, fromTo) <- getArgs >>= parseArgvOrThrow today

  let layoutFn = case optLayout config of
        Grid _ -> layoutGrid
        Flow _ -> layoutFlow

  putDoc $ layoutFn config fromTo today
