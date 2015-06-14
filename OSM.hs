module OSM where

import Data.Time (UTCTime)
import qualified Data.Map.Strict as Map

newtype NodeID = NodeID Int deriving (Show, Eq)
newtype WayID = WayID Int deriving (Show, Eq)
newtype RelationID = RelationID Int deriving (Show, Eq)

newtype RelationRole = RelationRole String deriving (Show, Eq)

data ElementID =
  ElementNodeID NodeID |
  ElementWayID WayID |
  ElementRelationID RelationID
  deriving (Show, Eq)

data RelationMember =
  RelationMember ElementID RelationRole
  deriving (Show, Eq)

newtype Latitude = Latitude Float deriving (Show, Eq)
newtype Longitude = Longitude Float deriving (Show, Eq)
newtype TagKey = TagKey String deriving (Show, Eq)

newtype Tags = Tags (Map.Map TagKey String) deriving (Show, Eq)

data Coordinates = Coordinates Latitude Longitude deriving (Show, Eq)

data VersionInfo = VersionInfo
  { user :: Maybe String
  , uid :: Maybe Integer
  , timestamp :: Maybe UTCTime
  , visible :: Maybe Bool
  , version :: Maybe Integer
  , changeset :: Maybe Integer
  } deriving (Show, Eq)

data Node = Node NodeID Tags Coordinates VersionInfo deriving (Show, Eq)
data Way = Way WayID Tags [Node] VersionInfo deriving (Show, Eq)
data Relation = Relation RelationID Tags [RelationMember] VersionInfo deriving (Show, Eq)
