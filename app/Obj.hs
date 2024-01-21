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
import Prettyprinter

import Control.Lens.TH

data Objective
  = LocalTag GitTag
  | TagOnGH GitTag
  | ReleaseOnGH ObjectiveReleaseOnGH
  | FlakeOutputBuilt Text
  deriving (Eq, Ord)

instance Pretty Objective where
  pretty (LocalTag tag)
    = "Local repo contains the tag " <> pretty tag
  pretty (TagOnGH tag)
    = "Repo at origin contains the tag " <> pretty tag
  pretty (ReleaseOnGH ghRel)
    = "The release \"" <> (pretty $ releaseOnGHTag ghRel) <> "\" is on GitHub"
  pretty (FlakeOutputBuilt ref)
    = "The flake output" <> pretty ref <> " is built"

data ObjectiveReleaseOnGH = ObjectiveReleaseOnGH
  { _objectiveReleaseOnGHReleaseId                          :: ReleaseId
  , _objectiveReleaseOnGHTagPrefix                          :: Text
  , _objectiveReleaseOnGHTitlePrefix                        :: Text
  , _objectiveReleaseOnGHDescription                        :: Text
  , _objectiveReleaseOnGHIncludeGithubGeneratedReleaseNotes :: Bool
  , _objectiveReleaseOnGHAssets                             :: (Map Text FlakeOutputPath)
  }
  deriving (Eq, Ord)

releaseOnGHTitle :: ObjectiveReleaseOnGH -> Text
releaseOnGHTitle ghRel
  = _objectiveReleaseOnGHTitlePrefix ghRel
  <> (renderReleaseId $ _objectiveReleaseOnGHReleaseId ghRel)

releaseOnGHTag :: ObjectiveReleaseOnGH -> GitTag
releaseOnGHTag ghRel = GitTag
  { _gitTagReleaseId = _objectiveReleaseOnGHReleaseId ghRel
  , _gitTagPrefix = _objectiveReleaseOnGHTagPrefix ghRel
  }

makeFields ''ObjectiveReleaseOnGH
