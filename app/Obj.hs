{-# LANGUAGE ScopedTypeVariables #-}

module Obj where

import Sys

import Config (FlakeOutputPath)
import Data.Map
import Data.Text
import Prettyprinter

data Objective
  = LocalTag GitTag
  | TagOnGH GitTag
  | ReleaseOnGH GitTag Text (Map Text FlakeOutputPath)
  | FlakeOutputBuilt Text
  deriving (Eq, Ord, Show)


objectiveDescription :: Objective -> Doc ann
objectiveDescription (LocalTag tag)       = "HEAD commit tagged with" <> pretty tag
objectiveDescription (TagOnGH tag)        = "Tag " <> pretty tag <> " on GitHub"
objectiveDescription (ReleaseOnGH _ _ _)  = "TODO"
objectiveDescription (FlakeOutputBuilt _) = "TODO"
