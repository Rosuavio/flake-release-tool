module Action where

import System.Process.Typed

import Data.ByteString.Lazy qualified as BS

type Action = IO (ExitCode, BS.ByteString)

tagHeadWith :: String -> IO (ExitCode, BS.ByteString)
tagHeadWith tagName = readProcessStdout . shell $
  "git tag -a " ++ tagName

