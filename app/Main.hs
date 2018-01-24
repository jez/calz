{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import           Data.List                                 (intercalate)
import qualified Data.Text                                 as T
import           Data.Text.Prettyprint.Doc.Render.Terminal (putDoc)
import           Data.Time
import           System.Console.Docopt
import           System.Environment                        (getArgs)

import           Calz.DateUtil
import           Calz.Formatter
import           Calz.Parser
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

Phrase:
  calz <month> [<year>]
  calz <year>
  calz (last|this|next) (month|year)
  calz <n> (months|years) ago
  calz <n> (months|years) from (now|today)
  calz from <phrase>... to <phrase>...

Examples:
  dec 2017
  next month
  3 months ago
  from 2 months from now to next year
|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

exitMsg :: String -> IO a
exitMsg = exitWithUsageMessage patterns

main :: IO ()
main = do
  args <- parseArgsOrExit patterns =<< getArgs

  -- helpers
  let hasOption opt = isPresent args (longOption opt)

  -- layout has two dependent options
  layout <- getArgOrExit args (longOption "layout") >>= \case
    "grid" -> Grid . read <$> getArgOrExit args (longOption "columns")
    "flow" -> return . Flow $ hasOption "separators"
    val    -> exitMsg $ "--layout must be 'flow' or 'grid'; found: " ++ val

  -- simple boolean options
  let color      = not $ hasOption "no-color"
  let hideLabels = hasOption "no-labels"
  let hidePad    = hasOption "no-pad"

  -- Done with config
  let config     = Config layout color hideLabels hidePad

  -- Parse phrase to get month range
  today <- localDay . zonedTimeToLocalTime <$> getZonedTime

  let phrase = T.pack $ intercalate " " . getAllArgs args $ argument "phrase"
  fromTo <- if getArgCount args (argument "phrase") == 0
    then
      let start = thisMonth today
          end   = addMonth start
      in  return $ DatePhrase start end
    else case parsePhrase today phrase of
      Left err -> do
        print err
        exitWithUsage patterns
      Right ft -> return ft

  putDoc $ runFormatCalendar config fromTo today
