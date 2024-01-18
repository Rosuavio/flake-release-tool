module Indicator where

import Config
import Obj
import Sys

import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NeM
import Data.Maybe
import Data.Set qualified as Set
import Data.Text

import Control.Lens

data Indicator
  = AlwaysPushTag
  | AlwaysCreateGithuRelease
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
    then Just $ ReleaseOnGH (GitTag releaseId)
    else Nothing

prettyUserObjectives :: NeM.NEMap Objective (Set.Set Indicator) -> String
prettyUserObjectives = flip NeM.foldlWithKey [] $ \curr key val ->
  curr ++ "(" ++ (show key) ++ ":" ++ show val ++ ")"

