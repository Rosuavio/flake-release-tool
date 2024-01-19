module Indicator where

import Config
import Obj (Objective (..), ObjectiveReleaseOnGH (..))
import Sys

import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NeM
import Data.Maybe
import Data.Set qualified as Set
import Data.Text
import Prettyprinter

import Control.Lens

data Indicator
  = AlwaysPushTag
  | AlwaysCreateGithuRelease
  | AssetsToPublish
  deriving (Enum, Bounded, Eq, Ord, Show)

getUserObjectives :: Text -> ReleaseConfig -> M.Map Objective (Set.Set Indicator)
getUserObjectives releaseId config = M.fromListWith Set.union
  $ mapMaybe
      (\i -> fmap (\obj -> (obj, Set.singleton i)) $ indicatorCheckConfig i releaseId config)
      [minBound..maxBound]

indicatorCheckConfig :: Indicator -> Text -> ReleaseConfig -> Maybe Objective
indicatorCheckConfig AlwaysPushTag releaseId c =
  if c ^. git . tag . alwaysPublish
    then Just $ TagOnGH (GitTag releaseId)
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

mkReleaseOnGH :: Text -> ReleaseConfig -> Objective
mkReleaseOnGH releaseId c = ReleaseOnGH $ ObjectiveReleaseOnGH
  { _objectiveReleaseOnGHTag = GitTag releaseId
  , _objectiveReleaseOnGHTitlePrefix = c ^. title . prefix
  , _objectiveReleaseOnGHDescription = c ^. description . text
  , _objectiveReleaseOnGHIncludeGithubGeneratedReleaseNotes = c ^. description . includeGithubGeneratedReleaseNotes
  , _objectiveReleaseOnGHAssets = c ^. gitHub . release . assets
  }
