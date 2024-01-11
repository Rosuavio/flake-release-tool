{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Foo where

import GHC.Generics

import Data.Default
import Data.YAML

import Control.Lens.TH

data ReleaseConfig = ReleaseConfig
  { _releaseConfigGit :: !ReleaseConfigGit
  }
  deriving stock Generic
  deriving anyclass Default

instance FromYAML ReleaseConfig where
  parseYAML = withMap "ReleaseConfig" $ \m -> ReleaseConfig
    <$> m .:! "git" .!= def

data ReleaseConfigGit = ReleaseConfigGit
  { _releaseConfigGitTag :: !ReleaseConfigGitTag
  }
  deriving stock Generic
  deriving anyclass Default

instance FromYAML ReleaseConfigGit where
  parseYAML = withMap "ReleaseConfigGit" $ \m -> ReleaseConfigGit
    <$> m .:! "tag" .!= def

data ReleaseConfigGitTag = ReleaseConfigGitTag
  { _releaseConfigGitTagAlwaysPublish :: !Bool
  }

instance FromYAML ReleaseConfigGitTag where
  parseYAML = withMap "Tag" $ \v -> ReleaseConfigGitTag
    <$> v .:! "always-publish" .!= (_releaseConfigGitTagAlwaysPublish $ def @ReleaseConfigGitTag)

instance Default ReleaseConfigGitTag where
  def = ReleaseConfigGitTag
    { _releaseConfigGitTagAlwaysPublish = False
    }

makeFields ''ReleaseConfig
makeFields ''ReleaseConfigGit
makeFields ''ReleaseConfigGitTag
