{-# LANGUAGE ScopedTypeVariables #-}

module ObjCheck where

import Change
import Checks
import Obj
import Sys

import Data.Map.NonEmpty qualified as NeM
import Data.Set qualified as S
import Data.Set.NonEmpty qualified as NeS

data ObjectiveCheckResult
  = Achived
  | Achievable Change
  | NotAchievable
  deriving Show

objectiveCheck :: Objective -> IO ObjectiveCheckResult
objectiveCheck (LocalTag tag) = do
  mCommit <- getCommitOfTag tag
  case mCommit of
    Nothing -> pure $ Achievable $ CreateLocalTag tag
    Just tagCommit -> do
      matchesHead <- checkGitCommitMatchesHead tagCommit
      pure $ case matchesHead of
        True  -> Achived
        False -> NotAchievable
objectiveCheck (TagOnGH tag) = do
  mRemoteCommit <- getRemoteRef tag
  case mRemoteCommit of
    Nothing -> pure $ Achievable $ PushTagToOrigin tag
    Just commitOfTag -> do
      matchesHead <- checkGitCommitMatchesHead commitOfTag
      pure $ case matchesHead of
        True  -> Achived
        False -> NotAchievable
objectiveCheck ReleaseOnGH = pure NotAchievable

changePreConditions :: Change -> S.Set Objective
changePreConditions (CreateLocalTag _)    = S.empty
changePreConditions (PushTagToOrigin tag) = S.singleton $ LocalTag tag

evalAllObjectives
  :: NeS.NESet Objective
  -> IO (NeM.NEMap Objective ObjectiveCheckResult)
evalAllObjectives objectives = do
  initalCheckedObjs <- checkObjs objectives

  go (getPrereqs initalCheckedObjs) initalCheckedObjs
  where
    -- TODO: Can simplify a bit if we switch to non-empty set of objs and normal
    -- map checkedObjs.
    go :: S.Set Objective
       -> NeM.NEMap Objective ObjectiveCheckResult
       -> IO (NeM.NEMap Objective ObjectiveCheckResult)
    go objs checkedObjs =
      let
        unCheckedObjs = S.difference
          objs
          (NeS.toSet $ NeM.keysSet checkedObjs)
      in case NeS.nonEmptySet unCheckedObjs of
        Nothing -> pure checkedObjs
        Just nonEmptyUnCheckedObjs -> do
          newCheckedObjs <- checkObjs nonEmptyUnCheckedObjs
          go
            (getPrereqs newCheckedObjs)
            (NeM.union checkedObjs newCheckedObjs)

checkObjs :: NeS.NESet Objective -> IO (NeM.NEMap Objective ObjectiveCheckResult)
checkObjs = sequenceA . NeM.fromSet objectiveCheck

getPrereqs :: NeM.NEMap Objective ObjectiveCheckResult -> S.Set Objective
getPrereqs = S.unions
  . S.map changePreConditions
  . getChanges

getChanges :: NeM.NEMap Objective ObjectiveCheckResult -> S.Set Change
getChanges  = S.fromList
  . mapMaybe' rezToMaybeChange
  . NeM.elems

rezToMaybeChange :: ObjectiveCheckResult -> Maybe Change
rezToMaybeChange a = case a of
  Achievable c -> Just c
  _            -> Nothing

mapMaybe' :: Foldable f => (a -> Maybe b) -> f a -> [b]
mapMaybe' f = foldr g []
  where
    g x rest
      | Just y <- f x = y : rest
      | otherwise     = rest
