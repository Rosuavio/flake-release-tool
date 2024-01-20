module Indicator where

import Config
import Obj (Objective (..), ObjectiveReleaseOnGH (..))
import Sys
import Util

import Data.Map qualified as M
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NeM
import Data.Maybe
import Data.Set.NonEmpty (NESet)
import Data.Set.NonEmpty qualified as NeS
import Prettyprinter

import Control.Lens

data Indicator
  = AlwaysPushTag
  | AlwaysCreateGithuRelease
  | AssetsToPublish
  deriving (Enum, Bounded, Eq, Ord, Show)

getUserObjectives
  :: ReleaseId
  -> ReleaseConfig
  -> Maybe (NEMap Objective (NESet Indicator))
getUserObjectives releaseId config = NeM.nonEmptyMap
  . M.fromListWith NeS.union
  $ mapMaybe
      (\i -> fmap (\obj -> (obj, NeS.singleton i)) $ indicatorCheckConfig i releaseId config)
      [minBound @Indicator ..maxBound @Indicator]

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

prettyUserObjectives :: NEMap Objective (NESet Indicator) -> Doc ann
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
