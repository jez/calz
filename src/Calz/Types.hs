module Calz.Types where

import           Data.Time

data Layout
  = Grid { columns :: Int }
  | Flow { separators :: Bool }
  deriving Show

data Config = Config
  { optLayout     :: Layout
  , optColor      :: Bool
  , optHideLabels :: Bool
  , optHidePad    :: Bool
  }
  deriving Show

defaultConfig :: Config
defaultConfig = Config
  { optLayout     = Flow False
  , optColor      = True
  , optHideLabels = False
  , optHidePad    = False
  }

data DatePhrase = DatePhrase
  { monthFrom :: Day -- ^ Inclusive
  , monthTo   :: Day -- ^ Exclusive
  }
  deriving (Show, Eq)

data Annotation
  = HeaderAnn
  | LabelAnn
  | TodayAnn
  | PaddedAnn
  | EndOfMonthAnn
