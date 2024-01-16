{-# LANGUAGE ScopedTypeVariables #-}
module Change where

import Action
import Checks
import Sys

data Change
  = CreateLocalTag GitTag
  | PushTagToOrigin GitTag
  deriving (Eq, Ord, Show)

changeActions :: Change -> [Action]
changeActions (CreateLocalTag tag) = [ tagHeadWith tag ]

changeChecks :: Change -> [ Check ]
changeChecks (CreateLocalTag tag) = [ checkGitTagIsOfHead tag ]
