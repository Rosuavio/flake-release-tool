{-# LANGUAGE ScopedTypeVariables #-}
module Change where

import Action
import Checks
import Config (FlakeOutputPath)
import Sys

import Data.Map
import Data.Text
import System.Process.Typed

data Change
  = CreateLocalTag GitTag
  | PushTagToOrigin GitTag
  | CreateReleaseOnGH GitTag Text (Map Text FlakeOutputPath)
  | BuildFlakeOuput Text
  deriving (Eq, Ord, Show)

changeActions :: Change -> [Action]
changeActions (CreateLocalTag tag)           = [ tagHeadWith tag ]
changeActions (PushTagToOrigin tag)          = [ pushGitTag tag ]
changeActions (CreateReleaseOnGH tag description assets) = [ createReleaseOnGH tag description assets ]
changeActions (BuildFlakeOuput flakeOutput)  = [ buildFlakeOuput flakeOutput ]

changeChecks :: Change -> [ Check ]
changeChecks (CreateLocalTag tag)        = [ checkGitTagIsOfHead tag ]
changeChecks (PushTagToOrigin tag)       = [ checkRemoteTagMatchedLocal tag ]
changeChecks (CreateReleaseOnGH tag _ _) = [ gitHubReleaseExsistsForTag tag ]
changeChecks (BuildFlakeOuput _)         = []

preformChange :: Change -> IO (Bool)
preformChange change = go (changeActions change)
  where
    go :: [Action] -> IO Bool
    go [] = return True
    go (next:rest) = do
      (exitCode, _output) <- next
      case exitCode of
        ExitSuccess -> go rest
        _           -> pure False
