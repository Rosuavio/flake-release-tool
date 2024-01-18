module Action where

import Sys
import System.Process.Typed

import Data.Text
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT

type Action = IO (ExitCode, Text)

