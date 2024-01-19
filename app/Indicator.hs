module Indicator where

import Config
import Obj (Objective (..), ObjectiveReleaseOnGH (..))
import Sys
import Util

import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NeM
import Data.Maybe
import Data.Set qualified as Set
import Prettyprinter

import Control.Lens

data Indicator
  = AlwaysPushTag
  | AlwaysCreateGithuRelease
  | AssetsToPublish
  deriving (Enum, Bounded, Eq, Ord, Show)

getUserObjectives :: ReleaseId -> ReleaseConfig -> M.Map Objective (Set.Set Indicator)
getUserObjectives releaseId config = M.fromListWith Set.union
  $ mapMaybe
      (\i -> fmap (\obj -> (obj, Set.singleton i)) $ indicatorCheckConfig i releaseId config)
      [minBound..maxBound]

indicatorCheckConfig :: Indicator -> ReleaseId -> ReleaseConfig -> Maybe Objective
indicatorCheckConfig AlwaysPushTag releaseId c =
  if c ^. git . tag . alwaysPublish
    then Just . TagOnGH $ GitTag releaseId (c ^. git . tag . prefix)
    else Nothing
indicatorCheckConfig AlwaysCreateGithuRelease releaseId c =
  if c ^. gitHub . release . alwaysPublish
    then Just $ mkReleaseOnGH releaseId c
    else Nothing
indicatorCheckConfig AssetsToPublish releaseId c =
  if not $ M.null (c ^. gitHub . release . assets)
    then Just $ mkReleaseOnGH releaseId c
    else Nothing

prettyUserObjectives :: NeM.NEMap Objective (Set.Set Indicator) -> Doc ann
prettyUserObjectives = viaShow

mkReleaseOnGH :: ReleaseId -> ReleaseConfig -> Objective
mkReleaseOnGH releaseId c = ReleaseOnGH $ ObjectiveReleaseOnGH
  { _objectiveReleaseOnGHReleaseId = releaseId
  , _objectiveReleaseOnGHTagPrefix = c ^. git . tag . prefix
  , _objectiveReleaseOnGHTitlePrefix = c ^. title . prefix
  , _objectiveReleaseOnGHDescription = c ^. description . text
  , _objectiveReleaseOnGHIncludeGithubGeneratedReleaseNotes = c ^. description . includeGithubGeneratedReleaseNotes
  , _objectiveReleaseOnGHAssets = c ^. gitHub . release . assets
  }
