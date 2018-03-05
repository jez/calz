module Calz.Layout.Grid (layoutGrid, layoutGridDebug) where

import           Control.Monad
import           Control.Monad.Reader
import           Data.List.Split                           (chunksOf)
import           Data.Maybe                                (fromMaybe)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Terminal
import           Data.Time

import           Calz.DateUtil
import           Calz.Layout.Util
import           Calz.Types

enumMonthsFrom :: Day -> [Day]
enumMonthsFrom from = from : (enumMonthsFrom (addMonth from))

enumMonthsFromTo :: Day -> Day -> [Day]
enumMonthsFromTo from to = takeWhile (<= to) $ enumMonthsFrom from

allMonthPhrases :: DatePhrase -> [DatePhrase]
allMonthPhrases (DatePhrase from to) =
  let monthStarts = enumMonthsFromTo from to
      ms          = zip monthStarts (drop 1 monthStarts)
  in  map (uncurry DatePhrase) ms


-- | Transpose, but record empty spots with @Nothing@.
--
-- If a value is in the third row in the input, we guarantee it ends up in the
-- third column in the output. Consider this matrix:
--
-- > [[10, 11],
-- >  [20],
-- >  [30, 31, 32]]
--
-- @jaggedTranspose@ outputs 31 and 32 in the third column by inserting padding
-- in the other columns:
--
-- > [[Just 10, Just 20, Just 30],
-- >  [Just 11, Nothing, Just 31],
-- >  [Nothing, Nothing, Just 32]]
--
-- By comparson, @transpose@ puts 31 in the /second/ column, and 32 in the
-- /first/ column:
--
-- > [[10, 20, 30],
-- >  [11, 31],
-- >  [32]]
jaggedTranspose :: [[a]] -> [[Maybe a]]
jaggedTranspose =
  let makeAllJust = (map . map $ Just) in jaggedTranspose' . makeAllJust

-- Like @join@, but with Maybe and List
--
-- @join@ requires that the two monads being joined are the same.
joinListMaybe :: Maybe [a] -> [a]
joinListMaybe Nothing   = []
joinListMaybe (Just []) = []
joinListMaybe (Just xs) = xs

jaggedTranspose' :: [[Maybe a]] -> [[Maybe a]]
jaggedTranspose' []       = []
jaggedTranspose' ([]:xss) = jaggedTranspose' xss
jaggedTranspose' ((x:xs):xss) =
  let xss' = map (joinListMaybe . tl) xss
      xs'  = if null xs && any ((> 0) . length) xss' then [Nothing] else xs
  in  (x : map (join . hd) xss) : jaggedTranspose' (xs' : xss')

-- The prettyprint library doesn't have a way to put arbitrary groups of lines
-- side by side. Instead, we have to manually interleave the lines.
--
-- The goal is that the raw data has the same shape as the characters that will
-- be displayed on the screen, instead of having each month be its own Doc.
--
-- We end up with /rows/ of month grids, rather than them all in a single list.
--
-- Interleaves weeks assuming there are n columns
-- interleaveMonthGrids :: Int -> [[[Day]]] -> [[[Maybe [Day]]]]
interleaveMonthGrids :: Int -> [[a]] -> [[[Maybe a]]]
interleaveMonthGrids n monthGrids =
  map jaggedTranspose . chunksOf n $ monthGrids

center :: Int -> String -> String
center n s =
  let len = length s
      d   = n - len
      r   = d `quot` 2
      l   = d - r
  in  if len >= n then s else replicate l ' ' ++ s ++ replicate r ' '

weekToDoc :: [Day] -> M (Doc Annotation)
weekToDoc days = hsep <$> mapM dayToDoc days

formatMonthPhrase :: DatePhrase -> M [Doc Annotation]
formatMonthPhrase monthPhrase = do
  hideLabels <- optHideLabels . formatConfig <$> ask

  let monthIdx = snd . getMonth . monthFrom $ monthPhrase
  let monthHeader =
        annotate LabelAnn
          . pretty
          . center (2 * 7 + 6)
          . getLongMonthName
          $ monthIdx
  let weekdaysHeader = annotate HeaderAnn $ pretty "Su Mo Tu We Th Fr Sa"

  -- Use runReader becasue we want to "modify" formatDatePhrase,
  -- but only for this call.
  r <- ask
  let r'       = r { formatDatePhrase = monthPhrase }
  let weekGrid = runReader (mapM weekToDoc . gridOfWeeks $ monthPhrase) r'

  if hideLabels
    then return $ (weekdaysHeader : weekGrid)
    else return $ (monthHeader : weekdaysHeader : weekGrid)

formatWeekDoc :: Maybe (Doc Annotation) -> Doc Annotation
formatWeekDoc = fromMaybe (hsep $ replicate 7 (space <> space))

formatWeekRow :: [Maybe (Doc Annotation)] -> Doc Annotation
formatWeekRow weekRow = concatWith twoSpaces $ map formatWeekDoc weekRow
  where twoSpaces x y = x <> space <> space <> y

-- TODO(jez) Show year if there are multiple years (or maybe just always?)
formatInterleavedWeeks :: [[Maybe (Doc Annotation)]] -> Doc Annotation
formatInterleavedWeeks weeks = (vsep $ map formatWeekRow weeks) <> line

formatCalendar :: M (Doc Annotation)
formatCalendar = do
  monthPhrases  <- allMonthPhrases . formatDatePhrase <$> ask
  monthDocGrids <- mapM formatMonthPhrase monthPhrases
  -- TODO(jez) This is a partial function!
  n             <- columns . optLayout . formatConfig <$> ask
  let interleavedWeeks = interleaveMonthGrids n monthDocGrids
  return $ (vsep $ map formatInterleavedWeeks interleavedWeeks) <> line

layoutGrid :: Config -> DatePhrase -> Day -> Doc AnsiStyle
layoutGrid config phrase today =
  runReader (formatCalendar >>= asAnsiStyle) (R config phrase today)

----- Only for debugging ------------------------------------------------------

dec01, jan21, mar01 :: Day
dec01 = fromGregorian 2017 12 01
jan21 = fromGregorian 2018 01 21
mar01 = fromGregorian 2018 03 01

layoutGridDebug :: Doc AnsiStyle
layoutGridDebug = layoutGrid
  (defaultConfig { optLayout = Grid {columns = 3} })
  (DatePhrase dec01 mar01)
  jan21
