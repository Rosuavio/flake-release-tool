{-# LANGUAGE ScopedTypeVariables #-}

module ObjGraph where

import Change
import Obj
import ObjCheck

import Data.Graph qualified as G
import Data.List.NonEmpty qualified as NeL
import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NeM
import Data.Maybe
import Data.Set qualified as S
import Data.Tuple.Extra
import Prettyprinter

data ReleaseGraphNode
  = NodeObjective ObjectiveCheckResult
  | NodeChange

data ReleaseGraphKey
  = KeyObjective Objective
  | KeyChange Change
  deriving (Eq, Ord)


graphFromObjectives
  :: NeM.NEMap Objective ObjectiveCheckResult
  -> ( G.Graph
     , G.Vertex -> (ReleaseGraphNode, ReleaseGraphKey, [ReleaseGraphKey])
     , ReleaseGraphKey -> Maybe G.Vertex
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
      . NeL.map (\(k,(n, to)) -> (n, k, S.toList to))
      . NeM.toList
      $ unionNeM changes objs

  in G.graphFromEdges fooo

unionNeM :: Ord a => M.Map a b -> NeM.NEMap a b -> NeM.NEMap a b
unionNeM m n = NeM.withNonEmpty n (<> n) m

prettyObjectiveGraph
  :: ( G.Graph
     , G.Vertex -> (ReleaseGraphNode, ReleaseGraphKey, [ReleaseGraphKey])
     , ReleaseGraphKey -> Maybe G.Vertex
     )
  -> Doc x
prettyObjectiveGraph (graph, _nodeFromVertex, _vertexFromKey) =
  let
    aaaaa = G.dff graph
  in
    viaShow aaaaa

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

prettyReleasePlan :: [ Change ] -> Doc x
prettyReleasePlan = viaShow

preformReleasePlan :: [Change] -> IO (Bool)
preformReleasePlan [] = pure True
preformReleasePlan (next:rest) = do
  rez <- preformChange next
  case rez of
    True  -> preformReleasePlan rest
    False -> pure False
