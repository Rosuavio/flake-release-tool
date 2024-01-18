module Action where

import System.Process.Typed

import Data.Text

type Action = IO (ExitCode, Text)

