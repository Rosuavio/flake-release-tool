{-# LANGUAGE ScopedTypeVariables #-}

module Obj where

import Sys

import Prettyprinter

data Objective
  = LocalTag GitTag
  | TagOnGH GitTag
  | ReleaseOnGH GitTag
  deriving (Eq, Ord, Show)


objectiveDescription :: Objective -> Doc ann
objectiveDescription (LocalTag tag)  = "HEAD commit tagged with" <> pretty tag
objectiveDescription (TagOnGH tag)   = "Tag " <> pretty tag <> " on GitHub"
objectiveDescription (ReleaseOnGH _) = "TODO"
