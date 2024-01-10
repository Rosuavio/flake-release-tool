module Main where

import MyLib qualified (someFunc)

import Options.Applicative

main :: IO ()
main = do
  g <- execParser opts
  putStrLn g
  MyLib.someFunc
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
