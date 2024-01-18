{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Config where

import GHC.Generics

import Data.Default
import Data.YAML

import Control.Lens.TH

data ReleaseConfig = ReleaseConfig
  { _releaseConfigGit    :: !ReleaseConfigGit
  , _releaseConfigGitHub :: !ReleaseConfigGitHub
  }
  deriving stock Generic
  deriving anyclass Default

instance FromYAML ReleaseConfig where
  parseYAML = withMap "ReleaseConfig" $ \m -> ReleaseConfig
    <$> m .:! "git" .!= def
    <*> m .:! "github" .!= def

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
  parseYAML = withMap "Tag" $ \m -> ReleaseConfigGitTag
    <$> m .:! "always-publish" .!= (_releaseConfigGitTagAlwaysPublish $ def @ReleaseConfigGitTag)

instance Default ReleaseConfigGitTag where
  def = ReleaseConfigGitTag
    { _releaseConfigGitTagAlwaysPublish = False
    }

data ReleaseConfigGitHub = ReleaseConfigGitHub
  { _releaseConfigGitHubRelease :: ReleaseConfigGitHubRelease
  }
  deriving stock Generic
  deriving anyclass Default

instance FromYAML ReleaseConfigGitHub where
  parseYAML = withMap "ReleaseConfigGitHub" $ \m -> ReleaseConfigGitHub
    <$> m .:! "release" .!= def

data ReleaseConfigGitHubRelease = ReleaseConfigGitHubRelease
  { _releaseConfigGitHubReleaseAlwaysPublish :: !Bool
  }

instance FromYAML ReleaseConfigGitHubRelease where
  parseYAML = withMap "ReleaseConfigGitHubRelease" $ \m -> ReleaseConfigGitHubRelease
    <$> m .:! "always-publish" .!= (_releaseConfigGitHubReleaseAlwaysPublish $ def @ReleaseConfigGitHubRelease)

instance Default ReleaseConfigGitHubRelease where
  def = ReleaseConfigGitHubRelease
    { _releaseConfigGitHubReleaseAlwaysPublish = False
    }

makeFields ''ReleaseConfig
makeFields ''ReleaseConfigGit
makeFields ''ReleaseConfigGitTag
makeFields ''ReleaseConfigGitHub
makeFields ''ReleaseConfigGitHubRelease
