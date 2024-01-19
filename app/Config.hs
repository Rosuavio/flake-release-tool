{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Config where

import GHC.Generics

import Data.Default
import Data.Map as M
import Data.Text as T
import Data.YAML

import Control.Lens.TH

data ReleaseConfig = ReleaseConfig
  { _releaseConfigGit         :: !ReleaseConfigGit
  , _releaseConfigGitHub      :: !ReleaseConfigGitHub
  , _releaseConfigDescription :: !ReleaseConfigDescription
  , _releaseConfigTitle       :: !ReleaseConfigTitle
  }
  deriving stock Generic
  deriving anyclass Default

instance FromYAML ReleaseConfig where
  parseYAML = withMap "ReleaseConfig" $ \m -> ReleaseConfig
    <$> m .:! "git" .!= def
    <*> m .:! "github" .!= def
    <*> m .:! "description" .!= def
    <*> m .:! "title" .!= def

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
  , _releaseConfigGitHubReleaseAssets        :: !(Map Text FlakeOutputPath)
  }

instance FromYAML ReleaseConfigGitHubRelease where
  parseYAML = withMap "ReleaseConfigGitHubRelease" $ \m -> ReleaseConfigGitHubRelease
    <$> m .:! "always-publish" .!= (_releaseConfigGitHubReleaseAlwaysPublish $ def @ReleaseConfigGitHubRelease)
    <*> m .:! "assets" .!= (_releaseConfigGitHubReleaseAssets $ def @ReleaseConfigGitHubRelease)

instance Default ReleaseConfigGitHubRelease where
  def = ReleaseConfigGitHubRelease
    { _releaseConfigGitHubReleaseAlwaysPublish = False
    , _releaseConfigGitHubReleaseAssets = M.empty
    }

data FlakeOutputPath = FlakeOutputPath
  { _flakeOutputPathFlakeOuput :: Text
  , _flakeOutputPathPath       :: Maybe Text
  }
  deriving stock (Eq, Ord, Show)

-- TODO: Figure out parseing individual strings as "FlakeOutputPath"
instance FromYAML FlakeOutputPath where
  parseYAML = withMap "FlakeOutputPath" $ \m -> FlakeOutputPath
    <$> m .: "output"
    <*> m .:! "path" .!= Nothing

data ReleaseConfigDescription = ReleaseConfigDescription
  { _releaseConfigDescriptionText                               :: !Text
  , _releaseConfigDescriptionIncludeGithubGeneratedReleaseNotes :: !Bool
  }

instance Default ReleaseConfigDescription where
  def = ReleaseConfigDescription T.empty False

instance FromYAML ReleaseConfigDescription where
  parseYAML = withMap "ReleaseConfigDescription" $ \m -> ReleaseConfigDescription
    <$> m .:! "text" .!= (_releaseConfigDescriptionText $ def @ReleaseConfigDescription)
    <*> m .:! "include-github-generated-release-notes" .!= (_releaseConfigDescriptionIncludeGithubGeneratedReleaseNotes $ def @ReleaseConfigDescription)

data ReleaseConfigTitle = ReleaseConfigTitle
  { _releaseConfigTitlePrefix :: !Text
  }

instance Default ReleaseConfigTitle where
  def = ReleaseConfigTitle T.empty

instance FromYAML ReleaseConfigTitle where
  parseYAML = withMap "ReleaseConfigTitle" $ \m -> ReleaseConfigTitle
    <$> m .:! "prefix" .!= (_releaseConfigTitlePrefix $ def @ReleaseConfigTitle)

makeFields ''ReleaseConfig
makeFields ''ReleaseConfigGit
makeFields ''ReleaseConfigGitTag
makeFields ''ReleaseConfigGitHub
makeFields ''ReleaseConfigGitHubRelease
makeFields ''FlakeOutputPath
makeFields ''ReleaseConfigDescription
makeFields ''ReleaseConfigTitle
