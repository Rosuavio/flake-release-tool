{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Config (ReleaseConfig)
import Indicator
import ObjCheck
import Util

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.ByteString.Lazy qualified as BS
import Data.Text
import Data.YAML (decode1, prettyPosWithSource)
import Options.Applicative
import Prettyprinter
import Prettyprinter.Render.Text
import System.Exit
import FastDownward
import FastDownward.Exec qualified as Exec

main :: IO ()
main = do
  rezz <- runProblem problem
  case rezz of
    Solved plan -> do
      putStrLn "Found a plan!"
      zipWithM_
        ( \i step -> putStrLn ( show i ++ ": " ++ show step ) )
        [ 1::Int .. ]
        ( totallyOrderedPlan plan )
    _ ->
      putStrLn "Couldn't find a plan!"

  releaseId <- execParser opts

  join . fmap (exitWith . either ExitFailure (const ExitSuccess)) . runExceptT $ do
    raw <- handleException @SomeException (BS.readFile "release.yaml") $ \_ -> do
      lift $ putStrLn "Error reading config file"
      throwE 2

    config <- onLeft (decode1 @ReleaseConfig raw) pure $ \(loc,emsg) -> do
      lift $ putStrLn ("release.yaml:" ++ prettyPosWithSource loc raw " error" ++ emsg)
      throwE 3

    userObjectives <- onNothing (getUserObjectives releaseId config) pure $ do
      lift $ putStrLn "No user objectives"
      throwE 0

    lift $ do
      putStrLn "Targeting the following user objectives for the specified reasons."
      putDoc $ pretty userObjectives
      putChar '\n'

    g <- handleException @SomeException
      (evalObjectiveGraph $ objectiveFromIndicatedObjectives userObjectives)
      $ \_ -> do
        lift $ putStrLn "Error evalutating objective graph"
        throwE 5

    lift $ do
      putStrLn "Release Graph"
      putDoc $ prettyObjectiveGraph g
      putChar '\n'

    releasePlan <- case getReleasePlan g of
      ReleaseAchivable relPlan -> pure relPlan
      ReleaseEmpty -> do
        lift $ do
          putStrLn "Empty release graph" -- Should not happen, fatal error
          putStrLn "Cannot preform release"
        throwE 6
      ReleaseNotAchievable -> do
        lift $ do
          putStrLn "There are release objectives not achievable by the release tool"
          putStrLn "Cannot preform release."
        throwE 7
      ReleaseAchived -> do
        lift $ putStrLn "Release already achieved."
        throwE 0

    lift $ do
      putStrLn "Release Plan"
      putDoc $ prettyReleasePlan releasePlan
      putChar '\n'

    rez <- handleException @SomeException (preformReleasePlan releasePlan) $ \_ -> do
      lift $ putStrLn "Error preforming release plan"
      throwE 9

    when (rez == False) $ do
      lift $ putStrLn "Release plan failed"
      throwE 10

    lift $ putStrLn "Release plan completed successfully"
  where
    opts = info (args <**> helper)
      ( fullDesc
      <> progDesc "Create a software release from a git repo using nix flakes"
      <> header "flake-release-tool - create software releases"
      <> failureCode 1
      )

    args :: Parser ReleaseId
    args = fmap (ReleaseId . pack) $ strArgument
      ( metavar "RELEASE_ID"
      <> help "The Release Identifier" )

    handleException :: Exception e => IO a -> (e -> ExceptT Int IO a) -> ExceptT Int IO a
    handleException f onError = ExceptT $ try f >>= \case
      Right r -> pure $ Right r
      Left e -> runExceptT $ onError e

    onNothing m j d = maybe d j m
    onLeft e r l = either l r e

type Foo = Var Text

problem :: Problem (SolveResult Text)
problem = do
  aaa <- newVar ("" :: Text)

  let
    ggg :: Effect Text
    ggg = do
      fff <- readVar aaa
      pure fff

  solve
    Exec.bjolp
    [ ggg ]
    [ aaa ?= "" ]
