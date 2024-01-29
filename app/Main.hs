{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Config (ReleaseConfig)
import Indicator
import ObjCheck
import Util

import Data.ByteString.Lazy qualified as BS
import Data.Text
import Data.YAML (decode1, prettyPosWithSource)
import Options.Applicative
import Prettyprinter
import Prettyprinter.Render.Text

main :: IO ()
main = do
  releaseId <- execParser opts
  raw <- BS.readFile "release.yaml"
  case decode1 @ReleaseConfig raw of
    Left (loc,emsg) -> putStrLn ("release.yaml:" ++ prettyPosWithSource loc raw " error" ++ emsg)
    Right c -> case getUserObjectives releaseId c of
      Nothing -> putStrLn "No user objectives"
      Just userObjectives -> do
        putStrLn "Targeting the following user objectives for the specified reasons."
        putDoc $ pretty userObjectives
        putChar '\n'

        g <- evalObjectiveGraph
          $ objectiveFromIndicatedObjectives userObjectives

        putStrLn "Release Graph"
        putDoc $ prettyObjectiveGraph g
        putChar '\n'

        case getReleasePlan g of
          ReleaseEmpty -> do
            putStrLn "Empty release graph" -- Should not happen, fatal error
            putStrLn "Cannot preform release"
          ReleaseNotAchievable -> do
            putStrLn "There are release objectives not achievable by the release tool"
            putStrLn "Cannot preform release."
          ReleaseAchived ->
            putStrLn "Release already achieved."
          ReleaseAchivable releasePlan -> do
            putStrLn "Release Plan"
            putDoc $ prettyReleasePlan releasePlan
            putChar '\n'

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
