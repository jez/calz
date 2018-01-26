module Calz.Layout.Flow (layoutFlow, layoutFlowDebug) where

import           Control.Monad.Reader
import           Data.Function                             ((&))
import           Data.List                                 (groupBy)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Time

import           Calz.DateUtil
import           Calz.Types
import           Calz.Layout.Util

-- | Get the month for a week
--
-- The month of a week is the month we'd display next to it in the calendar.
-- So if the month increments from say January to February this week, this
-- week's month is February.
getMonthOfWeek :: [Day] -> (Integer, Int)
getMonthOfWeek week = maximum $ map getMonth week

-- | Check if two weeks share the same month
--
-- Answers the question, "Do we have to put a new label next to the week2,
-- or can we re-use the label from week1?"
sameMonthOfWeek :: [Day] -> [Day] -> Bool
sameMonthOfWeek week1 week2 = getMonthOfWeek week1 == getMonthOfWeek week2

-- | Group a list of weeks into lists "months".
--
-- If a week contains the start of the month, all days in that week are
-- considered part of _that_ month, even if they technically belong to the
-- previous month. For example:
--
-- > [[[31 01 02 03 04 05 06],  -- January
-- >   [07 08 09 10 11 12 13],
-- >   [14 15 16 17 18 19 20],
-- >   [21 22 23 24 25 26 27]],
-- >  [[28 29 30 31 01 02 03],  -- February
-- >   [04 05 06 07 08 09 10],
-- >   [11 12 13 14 15 16 17],
-- >   [18 19 20 21 22 23 24]],
-- >  [[25 26 27 28 01 02 03]], -- March
-- > ]
groupWeeksByMonth :: [[Day]] -> [[[Day]]]
groupWeeksByMonth weeks = groupBy sameMonthOfWeek weeks

withMonth :: [[Day]] -> (String, [[Day]])
-- @month@ is non-empty because groupBy ensures each sublist is non-empty.
withMonth [] = error "A month ([[Day]]) must have at least one week ([Day])"
withMonth (month@(week:_)) =
  let monthNumber   = snd $ getMonthOfWeek week
      -- This (!!) operation is safe because month number is always 1 - 12
      -- and months has 12 months
      longMonghName = fst $ (months defaultTimeLocale) !! (monthNumber - 1)
  in  (longMonghName, month)

withMonths :: [[[Day]]] -> [(String, [[Day]])]
withMonths ms = map withMonth ms

monthNameFill :: Int
monthNameFill = maximum . map (length . fst) $ months defaultTimeLocale

formatMonth :: (String, [[Day]]) -> M (Doc Annotation)
formatMonth (monthName, monthGrid) = do
  hideLabels <- optHideLabels . formatConfig <$> ask
  grid       <- monthGrid & mapM (mapM dayToDoc >>^ hsep) >>^ vsep

  if hideLabels
    then return grid
    else do
      -- Don't show a label if there's only one week in this month grid
      let label = fill monthNameFill $ if length monthGrid == 1
            then emptyDoc
            else annotate LabelAnn $ pretty monthName
      return $ label <+> align grid

formatCalendar :: M (Doc Annotation)
formatCalendar = do
  phrase <- formatDatePhrase <$> ask
  let header =
        fill monthNameFill emptyDoc
          <+> (annotate HeaderAnn $ pretty "Su Mo Tu We Th Fr Sa")
  let grid =
        withMonths
          . groupWeeksByMonth
          . groupByWeek
          . allDays
          . padDatePhrase
          $ phrase
  (<+> line) . vsep . (header :) <$> (mapM formatMonth grid)

layoutFlow :: Config -> DatePhrase -> Day -> Doc AnsiStyle
layoutFlow config phrase today =
  runReader (formatCalendar >>= asAnsiStyle) (R config phrase today)

----- Only for debugging ------------------------------------------------------

jan01, jan21, mar01 :: Day
jan01 = fromGregorian 2018 01 01
jan21 = fromGregorian 2018 01 21
mar01 = fromGregorian 2018 03 01

layoutFlowDebug :: Doc AnsiStyle
layoutFlowDebug =
  layoutFlow defaultConfig (DatePhrase jan01 mar01) jan21