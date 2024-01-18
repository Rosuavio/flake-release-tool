{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Change
import Config (ReleaseConfig)
import Indicator
import Obj
import ObjCheck
import ObjGraph
import Sys

import Data.ByteString.Lazy qualified as BS
import Data.Foldable
import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NeM
import Data.Set qualified as S
import Data.Text
import Data.YAML (decode1, prettyPosWithSource)
import Options.Applicative
import Prettyprinter
import Prettyprinter.Render.Text

main :: IO ()
main = do
  releaseId <- fmap pack $ execParser opts
  raw <- BS.readFile "release.yaml"
  case decode1 @ReleaseConfig raw of
    Left (loc,emsg) -> putStrLn ("release.yaml:" ++ prettyPosWithSource loc raw " error" ++ emsg)
    Right c -> do
      let
        userObjectives = getUserObjectives releaseId c
      case NeM.nonEmptyMap userObjectives of
        Nothing -> putStrLn "No user objectives"
        Just nonEmptyUserObjectives -> do
          putStrLn "Detected user objectives..."
          putStrLn $ prettyUserObjectives nonEmptyUserObjectives

          allObjectives :: NeM.NEMap Objective ObjectiveCheckResult
            <- evalAllObjectives $ NeM.keysSet nonEmptyUserObjectives

          putStrLn "All release objectives"
          putDoc $ viaShow allObjectives

          putStrLn ""

          let
            g@(graph, nodeFromVertex, vertexFromKey) = graphFromObjectives allObjectives
            releasePlan = getReleasePlan (graph, nodeFromVertex)

          putDoc $ prettyObjectiveGraph g

          putStrLn ""

          putDoc $ prettyReleasePlan releasePlan

          rez <- preformReleasePlan releasePlan

          case rez of
            True  -> putStrLn "Release plan completed successfuly"
            False -> putStrLn "Release plan failed"
  where
    opts = info (args <**> helper)
      ( fullDesc
      <> progDesc "Create a software release from a git repo using nix flakes"
      <> header "flake-release-tool - create software releases"
      )

args :: Parser String
args = strArgument
  ( metavar "RELEASE_ID"
  <> help "The Release Identifier" )
