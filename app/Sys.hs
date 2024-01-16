module Sys where

import Data.Binary.Get
import Data.Map qualified as M
import Data.Map.NonEmpty qualified as NeM
import System.Process.Typed

import Data.ByteString qualified as SBS
import Data.ByteString.Lazy qualified as LBS

type GitTag = String

type GitRef = String

getCommitOfTag :: String -> IO (Maybe LBS.ByteString)
getCommitOfTag tag = do
  (code, output) <- readProcessStdout $ shell $ "git rev-list -n1 -i " ++ tag ++ " --"
  pure $ case code of
    ExitSuccess -> Just output
    _           -> Nothing


getCommitOfHead :: IO (Maybe LBS.ByteString)
getCommitOfHead = do
  (code, output) <- readProcessStdout $ "git rev-parse HEAD"
  pure $ case code of
    ExitSuccess -> Just output
    _           -> Nothing

gitFetch :: IO (Bool)
gitFetch = do
  (code, _) <- readProcessStdout $ "git fetch"
  pure $ case code of
    ExitSuccess -> True
    _           -> False

getRemoteRef :: String -> IO (Maybe SBS.ByteString)
getRemoteRef tag = do
  (code, output) <- readProcessStdout $ shell $ "git ls-remote origin " ++ targetRef
  pure $ case code of
    ExitSuccess ->
      let
        (commit, ref) = runGet aaa output
      in if targetRef == ref
        then Just commit
        else Nothing
    _           -> Nothing
  where
    targetRef :: SBS.ByteString
    targetRef = "refs/tags/" ++ tag
    aaa = do
      foo <- getByteString 40
      tt <- getByteString 1
      fdf <- getLazyByteStringNul
      return $ (foo, SBS.toStrict fdf)
