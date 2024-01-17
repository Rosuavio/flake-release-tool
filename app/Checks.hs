module Checks where

import Data.Text
import Sys

type Check = IO Bool

checkGitTagIsOfHead :: GitTag -> IO (Bool)
checkGitTagIsOfHead tag = do
  mTagCommit <- getCommitOfTag tag
  mHeadCommit <- getCommitOfHead
  pure $ case (mTagCommit, mHeadCommit) of
    (Just tagCommit, Just headCommit) -> tagCommit == headCommit
    _                                 -> False

checkGitCommitMatchesHead :: Text -> IO (Bool)
checkGitCommitMatchesHead commit = do
  mHeadCommit <- getCommitOfHead
  pure $ case mHeadCommit of
    Just headCommit -> commit == headCommit
    _               -> False
