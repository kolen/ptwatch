{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module OSM
  ( NodeID(..) , WayID(..), RelationID(..),
    RelationRole(..), ElementID(..), RelationMember(..),
    TagKey(..), Tags(..),
    Coordinates(..),
    Element(..), Node, Way, Relation,
    node, way, relation,
    coordinates, nodeIDs, members,
    Dataset(..)
  )

where

import Data.Int (Int64)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

newtype NodeID = NodeID Int64 deriving (Show, Eq, Ord, Num)
newtype WayID = WayID Int64 deriving (Show, Eq, Ord)
newtype RelationID = RelationID Int64 deriving (Show, Eq, Ord)
newtype RelationRole = RelationRole T.Text deriving (Show, Eq)

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
newtype TagKey = TagKey T.Text deriving (Show, Eq, Ord)

type Tags = Map.Map TagKey T.Text

-- Actually coordinates are not floats, but floats are suitable for
-- this app, it does not do much geometry calculation. Change to
-- proper data types if geometry calculations are needed.
--
-- "Latitude and Longitude are stored as scaled integers with a scale
-- factor of 1e7, so an integer latitude of -412870685 equates to
-- -41.2870685."
--
-- https://wiki.openstreetmap.org/wiki/Rails_port/Database_schema#Nodes
data Coordinates = Coordinates
  { latitude :: Float
  , longitude :: Float }
  deriving (Show, Eq)

data Element i p v = Element
  { id :: i
  , tags :: Tags
  , payload :: p
  , versionInfo :: v }
  deriving (Show, Eq, Ord)

type Node v = Element NodeID Coordinates v
type Way v = Element WayID [NodeID] v
type Relation v = Element RelationID [RelationMember] v

node :: NodeID -> Tags -> Coordinates -> Node ()
node i t c = Element i t c ()

way :: WayID -> Tags -> [NodeID] -> Way ()
way i t n = Element i t n ()

relation :: RelationID -> Tags -> [RelationMember] -> Relation ()
relation i t m = Element i t m ()

coordinates :: Node v -> Coordinates
coordinates = payload

nodeIDs :: Way v -> [NodeID]
nodeIDs = payload

members :: Relation v -> [RelationMember]
members = payload

data Dataset v = Dataset
  (Map.Map NodeID (Node v))
  (Map.Map WayID (Way v))
  (Map.Map RelationID (Relation v)) deriving (Show)
