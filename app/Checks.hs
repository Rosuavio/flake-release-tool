module Checks where

import Sys

import Data.ByteString.Lazy qualified as BS

type Check = IO Bool

checkGitTagIsOfHead :: String -> IO (Bool)
checkGitTagIsOfHead tag = do
  mTagCommit <- getCommitOfTag tag
  mHeadCommit <- getCommitOfHead
  pure $ case (mTagCommit, mHeadCommit) of
    (Just tagCommit, Just headCommit) -> tagCommit == headCommit
    _                                 -> False

checkGitCommitMatchesHead :: BS.ByteString -> IO (Bool)
checkGitCommitMatchesHead commit = do
  mHeadCommit <- getCommitOfHead
  pure $ case mHeadCommit of
    Just headCommit -> commit == headCommit
    _               -> False
