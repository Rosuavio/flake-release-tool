{-# LANGUAGE ScopedTypeVariables #-}

module ObjCheck where

import Change as C
import Checks
import Config (_flakeOutputPathFlakeOuput)
import Obj as O
import Sys

import Data.Graph qualified as G
import Data.List.NonEmpty qualified as NeL
import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NeM
import Data.Maybe
import Data.Set qualified as S
import Data.Set.NonEmpty qualified as NeS
import Data.Tuple.Extra
import Prettyprinter

import Control.Lens

data ObjectiveCheckResult
  = Achived
  | Achievable Change
  | NotAchievable
  deriving Eq

data ReleaseGraphNode
  = NodeObjective ObjectiveCheckResult
  | NodeChange

data ReleaseGraphKey
  = KeyObjective Objective
  | KeyChange Change
  deriving (Eq, Ord)

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
  rez <- gitHubReleaseExsistsForTag $ releaseOnGHTag obj
  pure $ case rez of
    -- TODO: Check if GitHub release matches the objective, if so determin that
    -- the Objective is Achived
    True  -> NotAchievable
    False -> Achievable . CreateReleaseOnGH $ ChangeCreateReleaseOnGH
      { _changeCreateReleaseOnGHReleaseId = obj ^. O.releaseId
      , _changeCreateReleaseOnGHTagPrefix = obj ^. O.tagPrefix
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
  = S.fromList $ (TagOnGH $ createReleaseOnGHTag obj)
  : (map
      (FlakeOutputBuilt .  _flakeOutputPathFlakeOuput)
      (M.elems $ obj ^. C.assets)
    )
changePreConditions (BuildFlakeOuput _)            = S.empty

evalObjectiveGraph
  :: NeS.NESet Objective
  -> IO ( G.Graph
     , G.Vertex -> (ReleaseGraphNode, ReleaseGraphKey, [ReleaseGraphKey])
     )
evalObjectiveGraph objectives = do
  o <- evalAllObjectives objectives
  pure $ graphFromObjectives o

canAchiveObjectives
  :: ( G.Graph
     , G.Vertex -> (ReleaseGraphNode, ReleaseGraphKey, [ReleaseGraphKey])
     )
  -> Bool
canAchiveObjectives (g, nodeFromVertex) =
  all
    (not . isNotAchievableObj . fst3 . nodeFromVertex)
    (G.vertices g)
  where
    isNotAchievableObj node = case node of
      NodeObjective NotAchievable -> True
      _                           -> False

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

graphFromObjectives
  :: NeM.NEMap Objective ObjectiveCheckResult
  -> ( G.Graph
     , G.Vertex -> (ReleaseGraphNode, ReleaseGraphKey, [ReleaseGraphKey])
     )
graphFromObjectives objectives =
  let
    changes :: M.Map ReleaseGraphKey (ReleaseGraphNode, S.Set ReleaseGraphKey)
    changes = M.mapKeysMonotonic KeyChange
      . M.fromSet (\c -> (NodeChange, S.mapMonotonic KeyObjective $ changePreConditions c))
      $ getChanges objectives

    objs :: NeM.NEMap ReleaseGraphKey (ReleaseGraphNode, S.Set ReleaseGraphKey)
    objs = NeM.map (\rez -> (NodeObjective rez, maybe S.empty S.singleton $ fmap KeyChange $ rezToMaybeChange rez))
      $ NeM.mapKeysMonotonic KeyObjective objectives

    fooo = NeL.toList
      . NeL.map (\(k,(n, t)) -> (n, k, S.toList t))
      . NeM.toList
      $ unionNeM changes objs

  in tupleDropThird $ G.graphFromEdges fooo
  where

unionNeM :: Ord a => M.Map a b -> NeM.NEMap a b -> NeM.NEMap a b
unionNeM m n = NeM.withNonEmpty n (<> n) m

prettyObjectiveGraph
  :: ( G.Graph
     , G.Vertex -> (ReleaseGraphNode, ReleaseGraphKey, [ReleaseGraphKey])
     )
  -> Doc ann
prettyObjectiveGraph (graph, _nodeFromVertex) =
  vsep . fmap prettyObjTree $ G.dff graph
  where
    prettyObjTree :: G.Tree G.Vertex -> Doc ann
    prettyObjTree (G.Node v []) = prettyVertex v
    prettyObjTree (G.Node v xs) = nest 2
      . vsep
      $ prettyVertex v : (map prettyObjTree xs)

    prettyVertex = prettyNode . tupleDropThird . _nodeFromVertex

    prettyNode :: (ReleaseGraphNode, ReleaseGraphKey) -> Doc ann
    prettyNode (NodeObjective Achived, KeyObjective k)
      = "O - " <> pretty k <>" - Achived"
    prettyNode (NodeObjective NotAchievable, KeyObjective k)
      = "O - " <> pretty k <> " - NotAchievable"
    prettyNode (NodeObjective (Achievable _), KeyObjective k)
      = "O - " <> pretty k <> " - Achievable"
    prettyNode (NodeChange, KeyChange k)
      = "C - " <> pretty k
    prettyNode _ = error "TODO"

getReleasePlan
  :: ( G.Graph
     , G.Vertex -> (ReleaseGraphNode, ReleaseGraphKey, [ReleaseGraphKey])
     )
  -> [ Change ]
getReleasePlan (g, nodeFromVertex) =
    mapMaybe (graphKeyToMaybeChange . snd3 . nodeFromVertex)
    $ G.reverseTopSort g

graphKeyToMaybeChange :: ReleaseGraphKey -> Maybe Change
graphKeyToMaybeChange rgk = case rgk of
  KeyChange c -> Just c
  _           -> Nothing

prettyReleasePlan :: [ Change ] -> Doc ann
prettyReleasePlan = pretty

preformReleasePlan :: [Change] -> IO (Bool)
preformReleasePlan [] = pure True
preformReleasePlan (next:rest) = do
  rez <- preformChange next
  case rez of
    True  -> preformReleasePlan rest
    False -> pure False

tupleDropThird :: (a, b, c) -> (a, b)
tupleDropThird (a, b, _) = (a, b)
