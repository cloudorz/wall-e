{-# LANGUAGE DeriveGeneric #-}
module Configuration where

import Data.Yaml.Config (loadYamlSettings, useEnv)
import Data.Yaml (FromJSON)
import GHC.Generics

data Config = Config { feeds :: [String]
                     } deriving (Generic, Show)

instance FromJSON Config

defaultConfigFilePath = "./config.yaml"

loadConfig :: FilePath -> IO Config
loadConfig path = loadYamlSettings [path] [] useEnv

