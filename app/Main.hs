{-# LANGUAGE LambdaCase #-}

module Main where

import           Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
import           Data.Time
import           Data.Version                              (showVersion)
import           System.Console.Docopt
import           System.Environment                        (getArgs)
import           System.Exit                               (exitSuccess)

import           Calz.ArgParser
import           Calz.Layout.Flow
import           Calz.Layout.Grid
import           Calz.Types
import           Paths_calz                                (version)

parseArgvOrThrow :: Day -> [String] -> IO (Config, DatePhrase)
parseArgvOrThrow today argv = case parseArgv today argv of
  Left (HelpError message) -> exitWithUsageMessage patterns message
  Left Version             -> do
    putStrLn . showVersion $ version
    exitSuccess
  Right parsed -> return parsed

main :: IO ()
main = do
  today            <- localDay . zonedTimeToLocalTime <$> getZonedTime
  (config, fromTo) <- getArgs >>= parseArgvOrThrow today

  let layoutFn = case optLayout config of
        Grid _ -> layoutGrid
        Flow _ -> layoutFlow

  putDoc $ layoutFn config fromTo today
