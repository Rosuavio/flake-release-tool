{-# LANGUAGE ScopedTypeVariables #-}

module Obj where

import Sys

import Data.Text

data Objective
  = LocalTag GitTag
  | TagOnGH GitTag
  | ReleaseOnGH
  deriving (Eq, Ord, Show)


objectiveDescription :: Objective -> Text
objectiveDescription (LocalTag tag) = "HEAD commit tagged with" <> tag
objectiveDescription (TagOnGH tag)  = "Tag " <> tag <> " on GitHub"
objectiveDescription ReleaseOnGH    = "TODO"
