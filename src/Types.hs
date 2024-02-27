{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import GHC.Generics

data DocumentData = DocumentData
  { documentContents :: String
  }
  deriving (Generic, ToJSON, FromJSON)

data DocumentSplitData = DocumentSplitData
  { contents :: [String]
  }
  deriving (Generic, ToJSON, FromJSON)
