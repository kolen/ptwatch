-- | Parse and encode JSON data in "Overpass API OSM JSON" format
--
-- http://overpass-api.de/output_formats.html#json

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module OSM.OverpassJSON
where

import Data.Int (Int64)
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified OSM
import qualified Data.Text as T
import Data.Scientific
import Data.HashMap.Strict as M

instance ToJSON OSM.NodeID where toJSON (OSM.NodeID i) = toJSON i
instance FromJSON OSM.NodeID where parseJSON v = OSM.NodeID <$> parseJSON v
instance ToJSON OSM.WayID where toJSON (OSM.WayID i) = toJSON i
instance FromJSON OSM.WayID where parseJSON v = OSM.WayID <$> parseJSON v
instance ToJSON OSM.RelationID where toJSON (OSM.RelationID i) = toJSON i
instance FromJSON OSM.RelationID where parseJSON v = OSM.RelationID <$> parseJSON v

instance ToJSON OSM.TagKey where toJSON (OSM.TagKey k) = toJSON k
instance FromJSON OSM.TagKey where parseJSON v = OSM.TagKey <$> parseJSON v
instance ToJSONKey OSM.TagKey
instance FromJSONKey OSM.TagKey

instance ToJSON OSM.RelationMember where
  toJSON (OSM.RelationMember id (OSM.RelationRole role)) = object
    [ "type" .= memberType id
    , "id" .= memberID id
    , "role" .= role ]
    where
      memberType :: OSM.ElementID -> T.Text
      memberType (OSM.ElementNodeID _) = "node"
      memberType (OSM.ElementWayID _) = "way"
      memberType (OSM.ElementRelationID _) = "relation"
      memberID (OSM.ElementNodeID (OSM.NodeID i)) = i
      memberID (OSM.ElementWayID (OSM.WayID i)) = i
      memberID (OSM.ElementRelationID (OSM.RelationID i)) = i
instance FromJSON OSM.RelationMember where
  parseJSON = withObject "RelationMember" $ \v ->
    let mkId =
          case M.lookup "type" v of
            Just "node" -> Right $ OSM.ElementNodeID . OSM.NodeID
            Just "way" -> Right $ OSM.ElementWayID . OSM.WayID
            Just "relation" -> Right $ OSM.ElementRelationID . OSM.RelationID
            Just a -> Left a
            Nothing -> Left Null
    in
      case mkId of
        Right mkId' -> OSM.RelationMember
          <$> (mkId' <$> (v .: "id"))
          <*> (OSM.RelationRole <$> v .: "role")
        Left val -> typeMismatch "RelationMemberType" val

instance ToJSON (OSM.Node ()) where
  toJSON n = object
    [ "type" .= String "node"
    , "id" .= OSM.id n
    , "tags" .= OSM.tags n
    , "lat" .= OSM.latitude (OSM.coordinates n)
    , "lon" .= OSM.longitude (OSM.coordinates n) ]
instance FromJSON (OSM.Node ()) where
  parseJSON = withObject "Node" $ \v -> OSM.node
    <$> v .: "id"
    <*> v .: "tags"
    <*> (OSM.Coordinates <$> v .: "lat" <*> v .: "lon")

instance ToJSON (OSM.Way ()) where
  toJSON w = object
    [ "type" .= String "way"
    , "id" .= OSM.id w
    , "tags" .= OSM.tags w
    , "nodes" .= OSM.nodeIDs w ]
instance FromJSON (OSM.Way ()) where
  parseJSON = withObject "Way" $ \v -> OSM.way
    <$> v .: "id"
    <*> v .: "tags"
    <*> v .: "nodes"

instance ToJSON (OSM.Relation ()) where
  toJSON r = object
    [ "type" .= String "relation"
    , "id" .= OSM.id r
    , "tags" .= OSM.tags r
    , "members" .= OSM.members r ]
