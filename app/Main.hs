{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Config (ReleaseConfig)
import Indicator
import ObjCheck
import Util

import Data.ByteString.Lazy qualified as BS
import Data.Map.NonEmpty qualified as NeM
import Data.Text
import Data.YAML (decode1, prettyPosWithSource)
import Options.Applicative
import Prettyprinter.Render.Text

main :: IO ()
main = do
  releaseId <- execParser opts
  raw <- BS.readFile "release.yaml"
  case decode1 @ReleaseConfig raw of
    Left (loc,emsg) -> putStrLn ("release.yaml:" ++ prettyPosWithSource loc raw " error" ++ emsg)
    Right c -> case getUserObjectives releaseId c of
      Nothing -> putStrLn "No user objectives"
      Just nonEmptyUserObjectives -> do
        putStrLn "Detected user objectives..."

        putDoc $ prettyUserObjectives nonEmptyUserObjectives

        putStrLn ""

        g@(graph, nodeFromVertex)
          <- evalObjectiveGraph $ NeM.keysSet nonEmptyUserObjectives

        let
          releasePlan = getReleasePlan (graph, nodeFromVertex)
          canPreformRelease = canAchiveObjectives g
        putDoc $ prettyObjectiveGraph g

        putStrLn ""

        case canPreformRelease of
          False -> do
            putStrLn "There are release objectives not achievable by the release tool"
            putStrLn "Cannot preform release."
          True -> do
            putDoc $ prettyReleasePlan releasePlan

            putStrLn ""

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

args :: Parser ReleaseId
args = fmap (ReleaseId . pack) $ strArgument
  ( metavar "RELEASE_ID"
  <> help "The Release Identifier" )
