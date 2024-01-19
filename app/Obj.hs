{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}

module Obj where

import Sys

import Config (FlakeOutputPath)
import Data.Map
import Data.Text
import Prettyprinter

import Control.Lens.TH

data Objective
  = LocalTag GitTag
  | TagOnGH GitTag
  | ReleaseOnGH ObjectiveReleaseOnGH
  | FlakeOutputBuilt Text
  deriving (Eq, Ord, Show)

data ObjectiveReleaseOnGH = ObjectiveReleaseOnGH
  { _objectiveReleaseOnGHTag                                :: GitTag
  , _objectiveReleaseOnGHDescription                        :: Text
  , _objectiveReleaseOnGHIncludeGithubGeneratedReleaseNotes :: Bool
  , _objectiveReleaseOnGHAssets                             :: (Map Text FlakeOutputPath)
  }
  deriving (Eq, Ord, Show)

objectiveDescription :: Objective -> Doc ann
objectiveDescription (LocalTag tag)       = "HEAD commit tagged with" <> pretty tag
objectiveDescription (TagOnGH tag)        = "Tag " <> pretty tag <> " on GitHub"
objectiveDescription (ReleaseOnGH _)      = "TODO"
objectiveDescription (FlakeOutputBuilt _) = "TODO"


makeFields ''ObjectiveReleaseOnGH
