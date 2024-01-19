{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Obj where

import Config (FlakeOutputPath)
import Sys
import Util

import Data.Map
import Data.Text

import Control.Lens.TH

data Objective
  = LocalTag GitTag
  | TagOnGH GitTag
  | ReleaseOnGH ObjectiveReleaseOnGH
  | FlakeOutputBuilt Text
  deriving (Eq, Ord, Show)

data ObjectiveReleaseOnGH = ObjectiveReleaseOnGH
  { _objectiveReleaseOnGHReleaseId                          :: ReleaseId
  , _objectiveReleaseOnGHTagPrefix                          :: Text
  , _objectiveReleaseOnGHTitlePrefix                        :: Text
  , _objectiveReleaseOnGHDescription                        :: Text
  , _objectiveReleaseOnGHIncludeGithubGeneratedReleaseNotes :: Bool
  , _objectiveReleaseOnGHAssets                             :: (Map Text FlakeOutputPath)
  }
  deriving (Eq, Ord, Show)

makeFields ''ObjectiveReleaseOnGH
