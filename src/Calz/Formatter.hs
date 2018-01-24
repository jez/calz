module Calz.Formatter (runFormatCalendar, runFormatCalendarDebug) where

import           Control.Monad.Reader
import           Data.Function                             ((&))
import           Data.List                                 (groupBy)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Time

import           Calz.DateUtil
import           Calz.Types

data R = R
  { formatConfig     :: Config
  , formatDatePhrase :: DatePhrase
  , formatToday      :: Day
  }
type M a = Reader R a

-- | Like @(.)@, but with a monadic action first
(^<<) :: Functor f => (b -> c) -> (a -> f b) -> (a -> f c)
(^<<) = (.) . fmap

-- | Like @flip (.)@, but with a monadic action first
(>>^) :: Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
(>>^) = flip (^<<)


getMonthOfWeek :: [Day] -> (Integer, Int)
getMonthOfWeek week = maximum $ map getMonth week

sameMonthOfWeek :: [Day] -> [Day] -> Bool
sameMonthOfWeek week1 week2 = getMonthOfWeek week1 == getMonthOfWeek week2

withMonth :: [[Day]] -> (String, [[Day]])
-- @month@ is non-empty because groupBy ensures each sublist is non-empty.
withMonth [] = error "A month ([[Day]]) must have at least one week ([Day])"
withMonth (month@(week:_)) =
  let monthNumber   = snd $ getMonthOfWeek week
      -- This (!!) operation is safe because month number is always 1 - 12
      -- and months has 12 months
      longMonghName = fst $ (months defaultTimeLocale) !! (monthNumber - 1)
  in  (longMonghName, month)

-- | Pad DatePhrase to start on a Sunday and end on a Saturday
padDatePhrase :: DatePhrase -> DatePhrase
padDatePhrase (DatePhrase from to) =
  -- The asymmetry is because @from@ is inclusive but @to@ is exclusive
  DatePhrase (firstOfSundayWeek from) (succ . lastOfSundayWeek $ pred to)

-- | List of all days within @DatePhrase@
--
-- [@monthFrom@, @monthTo@)
allDays :: DatePhrase -> [Day]
allDays (DatePhrase from to) = [from .. (pred to)]

-- | Group a list of days into weeks
groupByWeek :: [Day] -> [[Day]]
groupByWeek days = groupBy sameSundayWeek days

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

withMonths :: [[[Day]]] -> [(String, [[Day]])]
withMonths ms = map withMonth ms

monthNameFill :: Int
monthNameFill = maximum . map (length . fst) $ months defaultTimeLocale

dayToDoc :: Day -> M (Doc Annotation)
dayToDoc date = do
  let d   = getDay date
  let doc = if d < 10 then space <> pretty d else pretty d

  today <- formatToday <$> ask
  let doc' = if date == today then annotate TodayAnn doc else doc

  hidePad            <- optHidePad . formatConfig <$> ask
  DatePhrase from to <- formatDatePhrase <$> ask
  let doc'' = if not (from <= date && date < to)
        then if hidePad then pretty "  " else annotate PaddedAnn doc'
        else doc'

  let monthEnd = pred . nextMonth $ date
  let doc''' = if date >= from && diffDays monthEnd date < 7
        then annotate EndOfMonthAnn doc''
        else doc''

  return doc'''

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

removeSeparators :: Annotation -> [Annotation]
removeSeparators EndOfMonthAnn = []
removeSeparators x             = [x]

singleton :: a -> [a]
singleton x = [x]

annotationToAnsi :: Annotation -> [AnsiStyle]
annotationToAnsi HeaderAnn     = [colorDull Cyan]
annotationToAnsi LabelAnn      = [color Magenta]
annotationToAnsi TodayAnn      = [colorDull Yellow]
annotationToAnsi PaddedAnn     = [color Black <> bold]
annotationToAnsi EndOfMonthAnn = [underlined]

asAnsiStyle :: Doc Annotation -> M (Doc AnsiStyle)
asAnsiStyle doc = do
  useColor <- optColor . formatConfig <$> ask
  layout   <- optLayout . formatConfig <$> ask

  let useSeparators = case layout of
        Flow seps -> seps
        _         -> False

  let updateSeparators = if useSeparators then singleton else removeSeparators
  let translate        = if useColor then annotationToAnsi else const []

  return $ alterAnnotations translate . alterAnnotations updateSeparators $ doc

ansiFormatCalendar :: M (Doc AnsiStyle)
ansiFormatCalendar = formatCalendar >>= asAnsiStyle

runFormatCalendar :: Config -> DatePhrase -> Day -> Doc AnsiStyle
runFormatCalendar config phrase today =
  runReader ansiFormatCalendar (R config phrase today)

----- Only for debugging ------------------------------------------------------

jan01, jan21, mar01 :: Day
jan01 = fromGregorian 2018 01 01
jan21 = fromGregorian 2018 01 21
mar01 = fromGregorian 2018 03 01

runFormatCalendarDebug :: Doc AnsiStyle
runFormatCalendarDebug =
  runFormatCalendar defaultConfig (DatePhrase jan01 mar01) jan21
