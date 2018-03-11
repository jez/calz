{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Calz.PhraseParserSpec where

import           Data.Either       (either)
import qualified Data.Text         as T
import           Data.Time
import           Test.Hspec

import           Calz.PhraseParser
import           Calz.Types

jan01_17, dec01_17, jan01, feb01, jan21, mar01, oct01, nov01, dec01, jan01_19, feb01_19, mar01_19, jan01_20, feb01_20, mar01_20, jan01_21
  :: Day
jan01_17 = fromGregorian 2017 01 01
dec01_17 = fromGregorian 2017 12 01
jan01 = fromGregorian 2018 01 01
feb01 = fromGregorian 2018 02 01
jan21 = fromGregorian 2018 01 21
mar01 = fromGregorian 2018 03 01
oct01 = fromGregorian 2018 10 01
nov01 = fromGregorian 2018 11 01
dec01 = fromGregorian 2018 12 01
jan01_19 = fromGregorian 2019 01 01
feb01_19 = fromGregorian 2019 02 01
mar01_19 = fromGregorian 2019 03 01
jan01_20 = fromGregorian 2020 01 01
feb01_20 = fromGregorian 2020 02 01
mar01_20 = fromGregorian 2020 03 01
jan01_21 = fromGregorian 2021 01 01

good :: Day -> Day -> Maybe DatePhrase
good from to = Just $ DatePhrase from to

bad :: Maybe DatePhrase
bad = Nothing

testCases :: [(T.Text, Maybe DatePhrase)]
testCases =
  [ ("1"                            , good jan01 feb01)
  , ("10"                           , good oct01 nov01)
  , ("12 2018"                      , good dec01 jan01_19)
  , ("12"                           , good dec01 jan01_19)
  , ("122018"                       , bad)
  , ("122108"                       , bad)
  , ("2 2020"                       , good feb01_20 mar01_20)
  , ("2"                            , good feb01 mar01)
  , ("2018"                         , good jan01 jan01_19)
  , ("2020 "                        , good jan01_20 jan01_21)
  , ("2020"                         , good jan01_20 jan01_21)
  , ("220"                          , bad)
  , ("dec 2018"                     , good dec01 jan01_19)
  , ("feb 2019"                     , good feb01_19 mar01_19)
  , ("feb"                          , good feb01 mar01)
  , ("feb2019"                      , bad)
  , ("from last month to next month", good dec01_17 mar01)
  , ("from last month to this month", good dec01_17 feb01)
  , ("jan"                          , good jan01 feb01)
  , ("last  month"                  , good dec01_17 jan01)
  , ("last  year"                   , good jan01_17 jan01)
  , ("last month"                   , good dec01_17 jan01)
  , ("lastmonth"                    , bad)
  , ("next  year"                   , good jan01_19 jan01_20)
  , ("this  month"                  , good jan01 feb01)
  , ("this month"                   , good jan01 feb01)
  , ("thismonth"                    , bad)
  ]

toMaybe :: Either a b -> Maybe b
toMaybe = either (const Nothing) Just

inclExcl :: Maybe DatePhrase -> String
inclExcl Nothing = "Nothing"
inclExcl (Just (DatePhrase from to)) =
  "[" ++ (show from) ++ ", " ++ (show to) ++ ")"

runTestCase :: (T.Text, Maybe DatePhrase) -> SpecWith ()
runTestCase (phrase, expected) = do
  it ("'" ++ (T.unpack phrase) ++ "' -> '" ++ (inclExcl expected) ++ "'") $ do
    toMaybe (parsePhrase jan21 phrase) `shouldBe` expected

spec :: Spec
spec = do
  describe "parsePhrase" $ do
    mapM_ runTestCase testCases
