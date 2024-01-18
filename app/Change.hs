{-# LANGUAGE ScopedTypeVariables #-}
module Change where

import Action
import Checks
import Sys

import Data.Foldable
import Data.Traversable
import System.Process.Typed

data Change
  = CreateLocalTag GitTag
  | PushTagToOrigin GitTag
  deriving (Eq, Ord, Show)

changeActions :: Change -> [Action]
changeActions (CreateLocalTag tag)  = [ tagHeadWith tag ]
changeActions (PushTagToOrigin tag) = [ pushGitTag tag ]

changeChecks :: Change -> [ Check ]
changeChecks (CreateLocalTag tag)  = [ checkGitTagIsOfHead tag ]
changeChecks (PushTagToOrigin tag) = [ checkRemoteTagMatchedLocal tag ]

preformChange :: Change -> IO (Bool)
preformChange change = go (changeActions change)
  where
    go :: [Action] -> IO Bool
    go [] = return True
    go (next:rest) = do
      (exitCode, output) <- next
      case exitCode of
        ExitSuccess -> go rest
        _           -> pure False
