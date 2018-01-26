module Calz.Layout.Grid (layoutGrid, layoutGridDebug) where

import           Control.Monad
import           Data.List.Split                           (chunksOf)
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

allMonthGrids :: DatePhrase -> [[[Day]]]
allMonthGrids (DatePhrase from to) =
  let monthStarts = enumMonthsFromTo from to
      ms          = zip monthStarts (drop 1 monthStarts)
  in  map (gridOfWeeks . uncurry DatePhrase) ms


-- | Like @join@, but with Maybe and List
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

-- The prettyprint library doesn't have a way to put arbitrary groups of lines
-- side by side. Instead, we have to manually interleave the lines.
--
-- The goal is that the raw data has the same shape as the characters that will
-- be displayed on the screen, instead of having the each month be it's own Doc.
--
-- We end up with /rows/ of month grids, rather than them all in a single list.
--
-- Interleaves weeks assuming there are n columns
-- interleaveMonthGrids :: Int -> [[[Day]]] -> [[[Maybe [Day]]]]
interleaveMonthGrids :: Int -> [[a]] -> [[[Maybe a]]]
interleaveMonthGrids n monthGrids =
  map jaggedTranspose . chunksOf n $ monthGrids


-- TODO(jez) Finish grid layout
--
-- - [ ] Add month labels and day-of-week headers to each month
-- - [ ] Convert IR to Doc Annotation
-- - [ ] format with Ansi styles

layoutGrid :: Config -> DatePhrase -> Day -> Doc AnsiStyle
layoutGrid config phrase today =
  undefined config phrase today allMonthGrids interleaveMonthGrids

jan01, jan21, mar01 :: Day
jan01 = fromGregorian 2018 01 01
jan21 = fromGregorian 2018 01 21
mar01 = fromGregorian 2018 03 01

layoutGridDebug :: Doc AnsiStyle
layoutGridDebug = layoutGrid defaultConfig (DatePhrase jan01 mar01) jan21
