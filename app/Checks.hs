module Checks where

import Sys

type Check = IO Bool

checkGitTagIsOfHead :: GitTag -> IO (Bool)
checkGitTagIsOfHead tag = do
  mTagCommit <- getCommitOfTag tag
  mHeadCommit <- getCommitOfHead
  pure $ case (mTagCommit, mHeadCommit) of
    (Just tagCommit, Just headCommit) -> tagCommit == headCommit
    _                                 -> False

checkGitCommitMatchesHead :: CommitId -> IO (Bool)
checkGitCommitMatchesHead commit = do
  mHeadCommit <- getCommitOfHead
  pure $ case mHeadCommit of
    Just headCommit -> commit == headCommit
    _               -> False

checkRemoteTagMatchedLocal :: GitTag -> IO (Bool)
checkRemoteTagMatchedLocal tag = do
  mLocalCommit <- getCommitOfTag tag
  mRemoteCommit <- getCommitForRemoteTag tag
  pure $ case (mLocalCommit, mRemoteCommit) of
    (Just localCommit, Just remoteCommit) -> localCommit == remoteCommit
    _                                     -> False
