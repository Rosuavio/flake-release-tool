{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Sys where

import System.Process.Typed

import Data.Attoparsec.Text as P
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Prettyprinter

newtype GitTag = GitTag Text
  deriving newtype (Eq, Ord, Show, Pretty)
newtype CommitId = CommitId Text
  deriving newtype Eq
newtype RefId = RefId Text

getCommitOfTag :: GitTag -> IO (Maybe CommitId)
getCommitOfTag (GitTag tag) = do
  (code, stdout, _stderr) <- readProcess . shell . T.unpack
    $ "git rev-list -n1 -i " <> tag <> " --"
  pure $ case (code, parseOnly (P.takeWhile1 (not . isEndOfLine)) . LT.toStrict $ LT.decodeUtf8 stdout) of
    (ExitSuccess, Right commit) -> Just $ CommitId commit
    _                           -> Nothing

getCommitOfHead :: IO (Maybe CommitId)
getCommitOfHead = do
  (code, stdout, _stderr) <- readProcess . shell $ "git rev-parse HEAD"
  pure $ case (code, parseOnly (P.takeWhile1 (not . isEndOfLine)) . LT.toStrict $ LT.decodeUtf8 stdout) of
    (ExitSuccess, Right commit) -> Just $ CommitId commit
    _                           -> Nothing

-- TODO: Deal with anotated tags
getCommitForRemoteTag :: GitTag -> IO (Maybe CommitId)
getCommitForRemoteTag (GitTag tag) = do
  (code, stdout, _stderr) <- readProcess . shell . T.unpack $
    "git ls-remote origin " <> targetRef
  -- TODO: Catch decoding errors
  pure $ case (code, parseOnly getLsRemoteOutput . LT.toStrict $ LT.decodeUtf8 stdout) of
    (ExitSuccess, Right ((commit, ref):[])) ->
      if targetRef == ref
        then Just $ CommitId commit
        else Nothing
    _           -> Nothing
  where
    targetRef = "refs/tags/" <> tag
    getLsRemoteOutput = sepBy1' getLsRemoteLine endOfLine
    getLsRemoteLine = do
      a <- P.takeWhile1 ('\t' /= )
      _ <- char '\t'
      b <- P.takeWhile1 (not . isEndOfLine)
      return $ (a, b)

tagHeadWith :: GitTag -> IO (ExitCode, Text)
tagHeadWith (GitTag name) = do
  (code, stdout, _stderr) <- readProcess . shell . T.unpack $ "git tag " <> name
  pure $ (code, LT.toStrict $ LT.decodeUtf8 stdout)

pushGitTag :: GitTag -> IO (ExitCode, Text)
pushGitTag (GitTag name) = do
  (code, stdout, _stderr) <- readProcess . shell . T.unpack
    $ "git push origin refs/tags/" <> name
  pure $ (code, LT.toStrict $ LT.decodeUtf8 stdout)

gitHubReleaseExsistsForTag :: GitTag -> IO Bool
gitHubReleaseExsistsForTag (GitTag name) = do
  (code, _stdout, _stderr) <- readProcess . shell . T.unpack
    $ "gh release view " <> name
  pure $ case code of
    ExitSuccess -> True
    _           -> False

createReleaseOnGH :: GitTag -> IO (ExitCode, Text)
createReleaseOnGH (GitTag name) = do
  (code, stdout, _stderr) <- readProcess . shell . T.unpack
    $ "gh release create " <> name
  pure $ (code, LT.toStrict $ LT.decodeUtf8 stdout)
