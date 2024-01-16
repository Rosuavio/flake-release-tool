module Indicator where

import Config qualified as F
import Obj

import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NeM
import Data.Maybe
import Data.Set qualified as Set

import Control.Lens

data Indicator =
  AlwaysPushTag
  deriving (Enum, Bounded, Eq, Ord, Show)

getUserObjectives :: String -> F.ReleaseConfig -> M.Map Objective (Set.Set Indicator)
getUserObjectives releaseId config = M.fromListWith Set.union
  $ mapMaybe
      (\i -> fmap (\obj -> (obj, Set.singleton i)) $ indicatorCheckConfig i releaseId config)
      [minBound..maxBound]

indicatorCheckConfig :: Indicator -> String -> F.ReleaseConfig -> Maybe Objective
indicatorCheckConfig AlwaysPushTag releaseId c =
  if c ^. F.git . F.tag . F.alwaysPublish
    then Just $ TagOnGH releaseId
    else Nothing

prettyUserObjectives :: NeM.NEMap Objective (Set.Set Indicator) -> String
prettyUserObjectives = flip NeM.foldlWithKey [] $ \curr key val ->
  curr ++ "(" ++ (show key) ++ ":" ++ show val ++ ")"

