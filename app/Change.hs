{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell        #-}

module Change where

import Action
import Checks
import Config (FlakeOutputPath)
import Sys

import Data.Map
import Data.Text
import System.Process.Typed

import Control.Lens.TH

data Change
  = CreateLocalTag GitTag
  | PushTagToOrigin GitTag
  | CreateReleaseOnGH ChangeCreateReleaseOnGH
  | BuildFlakeOuput Text
  deriving (Eq, Ord, Show)

data ChangeCreateReleaseOnGH = ChangeCreateReleaseOnGH
  { _changeCreateReleaseOnGHTag                                :: GitTag
  , _changeCreateReleaseOnGHTitlePrefix                        :: Text
  , _changeCreateReleaseOnGHDescription                        :: Text
  , _changeCreateReleaseOnGHIncludeGithubGeneratedReleaseNotes :: Bool
  , _changeCreateReleaseOnGHAssets                             :: Map Text FlakeOutputPath
  }
  deriving (Eq, Ord, Show)

changeActions :: Change -> [Action]
changeActions (CreateLocalTag tag)           = [ tagHeadWith tag ]
changeActions (PushTagToOrigin tag)          = [ pushGitTag tag ]
changeActions (CreateReleaseOnGH c)          =
  [ createReleaseOnGH
      (_changeCreateReleaseOnGHTag c)
      (_changeCreateReleaseOnGHTitlePrefix c)
      (_changeCreateReleaseOnGHDescription c)
      (_changeCreateReleaseOnGHIncludeGithubGeneratedReleaseNotes c)
      (_changeCreateReleaseOnGHAssets c)
  ]
changeActions (BuildFlakeOuput flakeOutput)  = [ buildFlakeOuput flakeOutput ]

changeChecks :: Change -> [ Check ]
changeChecks (CreateLocalTag tag)  = [ checkGitTagIsOfHead tag ]
changeChecks (PushTagToOrigin tag) = [ checkRemoteTagMatchedLocal tag ]
changeChecks (CreateReleaseOnGH c) = [ gitHubReleaseExsistsForTag (_changeCreateReleaseOnGHTag c)]
changeChecks (BuildFlakeOuput _)   = []

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

makeFields ''ChangeCreateReleaseOnGH
