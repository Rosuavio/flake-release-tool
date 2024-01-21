module Indicator where

import Config
import Obj (Objective (..), ObjectiveReleaseOnGH (..))
import Sys
import Util

import Data.List.NonEmpty qualified as NeL
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
  deriving (Enum, Bounded, Eq, Ord)

instance Pretty Indicator where
  pretty AlwaysPushTag            = "git.tag.always-publish is True"
  pretty AlwaysCreateGithuRelease = "github.release.always-publish is True"
  pretty AssetsToPublish          = "github.release.assets contains assets"

newtype IndicatedObjectives = IndicatedObjectives (NEMap Objective (NESet Indicator))

instance Pretty IndicatedObjectives where
  pretty (IndicatedObjectives m)
    = vsep
    . NeL.toList
    . NeL.map prettyObjInd
    $ NeM.toAscList m
    where
      prettyObjInd (k, v)
        = nest 2
        . vsep
        $ pretty k
          : (NeL.toList . NeL.map pretty $ NeS.toAscList v)

objectiveFromIndicatedObjectives :: IndicatedObjectives -> NESet Objective
objectiveFromIndicatedObjectives (IndicatedObjectives m) = NeM.keysSet m

getUserObjectives
  :: ReleaseId
  -> ReleaseConfig
  -> Maybe (IndicatedObjectives)
getUserObjectives releaseId config = fmap IndicatedObjectives
  . NeM.nonEmptyMap
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

mkReleaseOnGH :: ReleaseId -> ReleaseConfig -> Objective
mkReleaseOnGH releaseId c = ReleaseOnGH $ ObjectiveReleaseOnGH
  { _objectiveReleaseOnGHReleaseId = releaseId
  , _objectiveReleaseOnGHTagPrefix = c ^. git . tag . prefix
  , _objectiveReleaseOnGHTitlePrefix = c ^. title . prefix
  , _objectiveReleaseOnGHDescription = c ^. description . text
  , _objectiveReleaseOnGHIncludeGithubGeneratedReleaseNotes = c ^. description . includeGithubGeneratedReleaseNotes
  , _objectiveReleaseOnGHAssets = c ^. gitHub . release . assets
  }
