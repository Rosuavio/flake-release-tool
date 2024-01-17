module Sys where

import System.Process.Typed

import Data.Attoparsec.Text as P
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT

type GitTag = Text

getCommitOfTag :: GitTag -> IO (Maybe Text)
getCommitOfTag tag = do
  (code, output) <- readProcessStdout . shell . T.unpack
    $ "git rev-list -n1 -i " <> tag <> " --"
  pure $ case code of
    ExitSuccess -> Just . LT.toStrict $ LT.decodeUtf8 output
    _           -> Nothing

getCommitOfHead :: IO (Maybe Text)
getCommitOfHead = do
  (code, output) <- readProcessStdout . shell $ "git rev-parse HEAD"
  pure $ case code of
    ExitSuccess -> Just . LT.toStrict $ LT.decodeUtf8 output
    _           -> Nothing

gitFetch :: IO (Bool)
gitFetch = do
  (code, _) <- readProcessStdout . shell $ "git fetch"
  pure $ case code of
    ExitSuccess -> True
    _           -> False

-- TODO: Deal with anotated tags
getRemoteRef :: GitTag -> IO (Maybe Text)
getRemoteRef tag = do
  (code, output) <- readProcessStdout . shell . T.unpack $
    "git ls-remote origin " <> targetRef
  -- TODO: Catch decoding errors
  case (code, parseOnly getLsRemoteOutput . LT.toStrict $ LT.decodeUtf8 output) of
    (ExitSuccess, Right ((commit, ref):[])) -> do
      putStrLn $ show commit
      putStrLn $ show ref
      pure $ if targetRef == ref
        then Just commit
        else Nothing
    (ExitSuccess, Left s) -> do
      putStrLn $ show s
      return Nothing
    _           -> pure $ Nothing
  where
    targetRef = "refs/tags/" <> tag
    getLsRemoteOutput = sepBy1' getLsRemoteLine endOfLine
    getLsRemoteLine = do
      a <- P.takeWhile1 ('\t' /= )
      _ <- char '\t'
      b <- P.takeWhile1 (not . isEndOfLine)
      return $ (a, b)
