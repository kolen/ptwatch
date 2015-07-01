{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module OSM where

import Data.Time (UTCTime)
import qualified Data.Map.Strict as Map

newtype NodeID = NodeID Integer deriving (Show, Eq, Ord)
newtype WayID = WayID Integer deriving (Show, Eq, Ord)
newtype RelationID = RelationID Integer deriving (Show, Eq, Ord)

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
newtype TagKey = TagKey String deriving (Show, Eq, Ord)

type Tags = Map.Map TagKey String

data Coordinates = Coordinates Latitude Longitude deriving (Show, Eq)

-- | Information about last modification, each field is optional
data VersionInfo = VersionInfo
  { user :: Maybe String -- ^ user name (user last modified this element)
  , uid :: Maybe Integer -- ^ user ID (user last modified this element)
  , timestamp :: Maybe UTCTime -- ^ timestamp (last modified time)
  , visible :: Maybe Bool -- ^ visible? (only deleted elements are invisible)
  , version :: Maybe Integer -- ^ version number
  , changeset :: Maybe Integer -- ^ changeset number
  } deriving (Show, Eq)

-- | Abstract OSM element with id "i" and payload "p". Contains fields common
--   to all OSM elements: id, tags, version info
data Ord i => Element i p = Element
  { id :: i
  , tags :: Tags
  , _payload :: p
  , versionInfo :: VersionInfo
} deriving (Show, Eq)

type Node     = Element NodeID Coordinates
type Way      = Element WayID [NodeID]
type Relation = Element RelationID [RelationMember]

nodeIds :: Way -> [NodeID]
nodeIds = _payload

data Dataset = Dataset (Map.Map NodeID Node)
                       (Map.Map WayID Way)
                       (Map.Map RelationID Relation) deriving (Show)
