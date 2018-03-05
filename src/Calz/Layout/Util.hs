module Calz.Layout.Util where

import           Control.Monad.Reader
import           Data.List                                 (groupBy)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Time

import           Calz.DateUtil
import           Calz.Types

----- Monad for layouting -----------------------------------------------------

-- This is just so we don't have to thread these three parameters through all
-- of our functions.

data R = R
  { formatConfig     :: Config
  , formatDatePhrase :: DatePhrase
  , formatToday      :: Day
  }
type M a = Reader R a


----- Helper combinators ------------------------------------------------------

-- | Like @(.)@, but with a monadic action first
--
-- Almost too clever for my own good. Best explained by analogy:
--
-- > (.)   ::              (b ->   c) -> (a ->   b) -> (a ->   c)
-- > (>>^) :: Functor f => (b ->   c) -> (a -> f b) -> (a -> f c)
-- > (>>=) ::   Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
--
-- So it's like a middle ground between @(.)@ and @(>>=)@ where the first
-- function is pure and the second is an action.
(^<<) :: Functor f => (b -> c) -> (a -> f b) -> (a -> f c)
(^<<) = (.) . fmap

-- | Flipped version of the above.
(>>^) :: Functor f => (a -> f b) -> (b -> c) -> (a -> f c)
(>>^) = flip (^<<)

-- | Safe list head
hd :: [a] -> Maybe a
hd []    = Nothing
hd (x:_) = Just x

-- | Safe list tail
tl :: [a] -> Maybe [a]
tl []     = Nothing
tl (_:xs) = Just xs


----- DatePhrases -------------------------------------------------------------

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

-- | Turn a @DatePhrase@ into a the grid for that range.
--
-- For a given @DatePhrase@, compute the least grid of full weeks (Sunday to
-- Saturday weeks) containing the start and end of the @DatePhrase@.
gridOfWeeks :: DatePhrase -> [[Day]]
gridOfWeeks = groupByWeek . allDays . padDatePhrase

-- | Get the month for a week
--
-- The month of a week is the month we'd display next to it in the calendar.
-- So if the month increments from say January to February this week, this
-- week's month is February.
getMonthOfWeek :: [Day] -> (Integer, Int)
getMonthOfWeek week = maximum $ map getMonth week

-- | Return the long month name (i.e., "January") for a given month by number.
--
-- Takes a number between 1 - 12. Fails otherwise.
getLongMonthName :: Int -> String
-- This (!!) operation is safe because month number is always 1 - 12
-- and months is length 12
getLongMonthName monthIdx = fst $ (months defaultTimeLocale) !! (monthIdx - 1)


----- Annotations -------------------------------------------------------------

-- | Annotate a single day given some config
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
  -- TODO(jez) Figure out a way to add EndOfMonthAnn anns between the days.
  let doc''' = if date >= from && diffDays monthEnd date < 7
        then annotate EndOfMonthAnn doc''
        else doc''

  return doc'''

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
