{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE QuasiQuotes      #-}

module Calz.ArgParser (parseArgv, patterns, ArgvParseError(..)) where

import           Control.Monad         (when)
import           Control.Monad.Except
import qualified Data.Text             as T
import           Data.Time
import           System.Console.Docopt

import           Calz.DateUtil
import           Calz.PhraseParser
import           Calz.Types

patterns :: Docopt
patterns =
  [docopt|
Display a calendar for a month or a range of months.

Usage:
  calz [options] [<phrase>...]

Options:
  -l, --layout=<layout>  Can be 'grid' or 'flow' [default: flow]
  -n, --columns=<n>      If layout is 'grid': how many columns to use
                         [default: 3]
  -s, --separators       If layout is 'flow': show month separators
  -C, --no-color         Disable all color
  -H, --no-labels        Don't show month labels
  -P, --no-pad           Complete the first and last weeks of every month with
                         the first and last days of surrounding months
  -h, --help             Show this help message
  -v, --version          Show version information

Phrase:
  calz <month> [<year>]
  calz <year>
  calz (last|this|next) (month|year)
  calz last <n> (months|years)
  calz <n> (months|years) ago
  calz next <n> (months|years)
  calz <n> (months|years) from (now|today)
  calz from <phrase>... to <phrase>...
  calz <phrase>... to <phrase>...

Examples:
  dec 2017
  next month
  3 months ago
  from 2 months from now to next year
|]

data ArgvParseError
  = HelpError String
  | Version

throwHelpError :: MonadError ArgvParseError m => String -> m a
throwHelpError = throwError . HelpError

parseArgsOrThrow :: MonadError ArgvParseError m => [String] -> m Arguments
parseArgsOrThrow argv = case parseArgs patterns argv of
  Left  _      -> throwHelpError ""
  Right result -> return result

hasOption' :: Arguments -> String -> Bool
hasOption' args opt = isPresent args (longOption opt)

getArgOrThrow' :: MonadError ArgvParseError m => Arguments -> Option -> m String
getArgOrThrow' args opt = case getArg args opt of
  Nothing     -> throwHelpError ""
  Just result -> return result

parseArgv
  :: MonadError ArgvParseError m => Day -> [String] -> m (Config, DatePhrase)
parseArgv today argv = do
  args <- parseArgsOrThrow argv

  -- helpers
  let hasOption     = hasOption' args
      getArgOrThrow = getArgOrThrow' args

  -- exit immediately if we see the '--help' flag
  when (hasOption "help")    (throwHelpError "")

  -- exit immediately if we see the '--version' flag
  when (hasOption "version") (throwError Version)

  -- layout has two dependent options (it would be nice if docopt were smart
  -- enough to know that this will never fail, so we could omit getArgOrThrow)
  layoutOpt <- getArgOrThrow (longOption "layout") >>= \case
    "grid" -> Grid . read <$> getArgOrThrow (longOption "columns")
    "flow" -> return . Flow $ hasOption "separators"
    val -> throwHelpError $ "--layout must be 'flow' or 'grid'; found: " ++ val

  -- simple boolean options
  let color      = not $ hasOption "no-color"
  let hideLabels = hasOption "no-labels"
  let hidePad    = hasOption "no-pad"

  -- Done with config
  let config     = Config layoutOpt color hideLabels hidePad

  -- Parse phrase to get month range
  let phrase = T.pack $ unwords . getAllArgs args $ argument "phrase"
  fromTo <- if getArgCount args (argument "phrase") == 0
    then
      let start = thisMonth today
          end   = addMonth start
      in  return $ DatePhrase start end
    else case parsePhrase today phrase of
      Left  _ -> throwHelpError $ "Could't parse phrase: " ++ T.unpack phrase
      Right x -> return x

  return (config, fromTo)
