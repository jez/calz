{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Calz.PhraseParser
  ( parsePhrase
  , parseWithEof
  )
where

import           Control.Monad.Reader
import           Data.Char            (toLower)
import qualified Data.Text            as T
import           Data.Time
import           Text.Parsec

import           Calz.DateUtil
import           Calz.Types

type M = ParsecT T.Text () (Reader Day)

january :: Int
january = 1

text :: Monad m => T.Text -> ParsecT T.Text u m ()
text t = string (T.unpack t) *> return ()

spaces1 :: Stream s m Char => ParsecT s u m ()
spaces1 = skipMany1 space

-- Collect information to make parsers for the three different month formats:
-- 1 / "January" / "Jan"
--
-- Input is like (1, ("January", "Jan"))
-- Output is like [(1, "1"), (1, "january"), (1, "jan")]
threeMonthReprs :: (Int, (String, String)) -> [(Int, String)]
threeMonthReprs (mi, (longMonth, shortMonth)) =
  [(mi, show mi), (mi, map toLower longMonth), (mi, map toLower shortMonth)]

allMonthParsers :: [M Int]
allMonthParsers =
  map (\(i, s) -> string s >> return i)
    -- If the month string matches, return the Int for this month
    . concatMap threeMonthReprs
    -- Backwards so that we try to match '10', '11', and '12' before '1'.
    . zip [12, 11 .. 1]
    . reverse
    . months
    $ defaultTimeLocale

month :: M Int
month = choice . map try $ allMonthParsers

year :: M Integer
year = read <$> count 4 digit

lastThisNext :: Integral a => M a
lastThisNext =
  try (text "last" >> return (-1)) <|> try (text "this" >> return 0) <|> try
    (text "next" >> return 1)

monthAndYearPhrase :: M DatePhrase
monthAndYearPhrase = do
  m <- month
  spaces1
  y <- year
  let start = firstDayOfMonth (y, m)
  let end   = addMonth start
  return $ DatePhrase start end

monthPhrase :: M DatePhrase
monthPhrase = do
  currYear <- asks getYear
  m        <- month
  let start = firstDayOfMonth (currYear, m)
  let end   = addMonth start
  return $ DatePhrase start end

yearPhrase :: M DatePhrase
yearPhrase = do
  y <- year
  let start = firstDayOfMonth (y, january)
  let end   = addYear start
  return $ DatePhrase start end

-- Alias for 'this month'
todayNow :: M DatePhrase
todayNow = do
  today <- ask
  _     <- try (text "today") <|> try (text "now")
  let start = thisMonth today
  let end   = addMonth start
  return $ DatePhrase start end

oneThruTen :: Integral a => M a
oneThruTen =
  choice
    . map (try . makeParser)
    $ [ ("one"  , 1)
      , ("two"  , 2)
      , ("three", 3)
      , ("four" , 4)
      , ("five" , 5)
      , ("six"  , 6)
      , ("seven", 7)
      , ("eight", 8)
      , ("nine" , 9)
      , ("ten"  , 0)
      ]
 where
  makeParser :: Integral a => (T.Text, a) -> M a
  makeParser (t, i) = text t >> return i

nUnits :: (Integral a, Read a) => T.Text -> M a
nUnits unit = do
  number <* spaces1 <* text unit <* optional (char 's')
 where
  number :: (Integral a, Read a) => M a
  number = try (read <$> many1 digit) <|> try oneThruTen

relativeUnit :: (Integral a, Read a) => T.Text -> M a
relativeUnit unit = do
  try (lastThisNext <* spaces1 <* text unit)
    <|> try (nUnits unit <* spaces1 <* text "from" <* spaces1 <* todayNow)
    <|> try (negate <$> nUnits unit <* spaces1 <* text "ago")

relativeMonths :: M DatePhrase
relativeMonths = do
  today  <- ask
  offset <- relativeUnit "month"
  let start = addGregorianMonthsClip offset . thisMonth $ today
  let end   = addMonth start
  return $ DatePhrase start end

relativeYears :: M DatePhrase
relativeYears = do
  currYear <- asks getYear
  offset   <- relativeUnit "year"
  let thisYear = firstDayOfMonth (currYear, january)
  let start = addGregorianMonthsClip (offset * 12) thisYear
  let end      = addYear start
  return $ DatePhrase start end

lastNextNMonths :: M DatePhrase
lastNextNMonths = do
  today  <- ask
  -- This is a clever trick: premultiply lastThisNext offset into 'n'
  -- (so nUnits can be forwards OR backwards)
  offset <- (*) <$> lastThisNext <*> (spaces1 *> nUnits "month")
  let thisMonth' = thisMonth today
  if offset < 0
    then do
      let start = addGregorianMonthsClip offset thisMonth'
      return $ DatePhrase start thisMonth'
    else do
      let end = addGregorianMonthsClip offset thisMonth'
      return $ DatePhrase thisMonth' end

lastNextNYears :: M DatePhrase
lastNextNYears = do
  currYear <- asks getYear
  -- This is a clever trick: premultiply lastThisNext offset into 'n'
  -- (so nUnits can be forwards OR backwards)
  offset   <- (*) <$> lastThisNext <*> (spaces1 *> nUnits "year")
  let thisYear = firstDayOfMonth (currYear, january)
  if offset < 0
    then do
      let start = addGregorianMonthsClip (offset * 12) thisYear
      return $ DatePhrase start thisYear
    else do
      let end = addGregorianMonthsClip (offset * 12) thisYear
      return $ DatePhrase thisYear end

simplePhrase :: M DatePhrase
simplePhrase =
  try todayNow
    -- these phrases are more complex than below (more likely to fail)
    <|> try relativeMonths
    <|> try relativeYears
    <|> try lastNextNMonths
    <|> try lastNextNYears
    -- year before month, because year *must* be length 4, so it's more specific
    <|> try yearPhrase
    <|> try monthAndYearPhrase
    <|> try monthPhrase

compoundPhrase :: M DatePhrase
compoundPhrase = do
  text "from" *> spaces1
  DatePhrase startStart _ <- simplePhrase
  spaces1 *> text "to" *> spaces1
  DatePhrase _ endEnd <- simplePhrase
  return $ DatePhrase startStart endEnd

simpleOrCompoundPhrase :: M DatePhrase
simpleOrCompoundPhrase = try simplePhrase <|> try compoundPhrase

parseWith :: Day -> M a -> T.Text -> Either ParseError a
parseWith today parser phrase =
  runReader (runParserT parser () "(unknown)" (T.toLower phrase)) today

-- | Just for testing; hard codes 18 Jan 2018 for convenience
parseWithEof :: M a -> T.Text -> Either ParseError a
parseWithEof parser = parseWith (fromGregorian 2018 01 18) (parser <* eof)

parsePhrase :: Day -> T.Text -> Either ParseError DatePhrase
parsePhrase today =
  parseWith today (spaces *> simpleOrCompoundPhrase <* spaces <* eof)
