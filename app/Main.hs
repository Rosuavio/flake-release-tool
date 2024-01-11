module Main where

import Foo

import Data.ByteString.Lazy qualified as BS
import Data.YAML (decode1, prettyPosWithSource)
import Options.Applicative

import Control.Lens

main :: IO ()
main = do
  g <- execParser opts
  raw <- BS.readFile "release.yaml"
  case decode1 @ReleaseConfig raw of
    Left (loc,emsg) -> do
     putStrLn ("release.yaml:" ++ prettyPosWithSource loc raw " error" ++ emsg)
    Right c -> do
      let
        pubGitTag = c^.git.tag.alwaysPublish
      putStrLn . show $ pubGitTag
      if pubGitTag
        then do
          foo <- pure True -- git tag exisits
          if foo
            then putStrLn "nothing to do"
            else putStrLn "git tag"
        else
          putStrLn "nothing to do"
  putStrLn g
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
