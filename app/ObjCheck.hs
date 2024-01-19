{-# LANGUAGE ScopedTypeVariables #-}

module ObjCheck where

import Change as C
import Checks
import Config (_flakeOutputPathFlakeOuput)
import Obj as O
import Sys

import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NeM
import Data.Set qualified as S
import Data.Set.NonEmpty qualified as NeS

import Control.Lens

data ObjectiveCheckResult
  = Achived
  | Achievable Change
  | NotAchievable
  deriving (Show, Eq)

objectiveCheck :: Objective -> IO ObjectiveCheckResult
objectiveCheck (LocalTag tagName) = do
  mCommit <- getCommitOfTag tagName
  case mCommit of
    Nothing -> pure $ Achievable $ CreateLocalTag tagName
    Just tagCommit -> do
      matchesHead <- checkGitCommitMatchesHead tagCommit
      pure $ case matchesHead of
        True  -> Achived
        False -> NotAchievable
objectiveCheck (TagOnGH tagName) = do
  mRemoteCommit <- getCommitForRemoteTag tagName
  case mRemoteCommit of
    Nothing -> pure $ Achievable $ PushTagToOrigin tagName
    Just remoteCommit -> do
      matchesHead <- checkGitCommitMatchesHead remoteCommit
      pure $ case matchesHead of
        True  -> Achived
        False -> NotAchievable
objectiveCheck (ReleaseOnGH obj) = do
  rez <- gitHubReleaseExsistsForTag $ (obj ^. O.tag)
  pure $ case rez of
    -- TODO: Check if GitHub release matches the objective, if so determin that
    -- the Objective is Achived
    True  -> NotAchievable
    False -> Achievable . CreateReleaseOnGH $ ChangeCreateReleaseOnGH
      { _changeCreateReleaseOnGHTag = obj ^. O.tag
      , _changeCreateReleaseOnGHTitlePrefix = obj ^. O.titlePrefix
      , _changeCreateReleaseOnGHDescription = obj ^. O.description
      , _changeCreateReleaseOnGHIncludeGithubGeneratedReleaseNotes = obj ^. O.includeGithubGeneratedReleaseNotes
      , _changeCreateReleaseOnGHAssets = obj ^. O.assets
      }
objectiveCheck (FlakeOutputBuilt flakeOutput) =
  pure $ Achievable $ BuildFlakeOuput flakeOutput

changePreConditions :: Change -> S.Set Objective
changePreConditions (CreateLocalTag _)             = S.empty
changePreConditions (PushTagToOrigin tagName)      = S.singleton $ LocalTag tagName
changePreConditions (CreateReleaseOnGH obj)
  = S.fromList $ TagOnGH (obj ^. C.tag)
  : (map
      (FlakeOutputBuilt .  _flakeOutputPathFlakeOuput)
      (M.elems $ obj ^. C.assets)
    )
changePreConditions (BuildFlakeOuput _)            = S.empty

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
