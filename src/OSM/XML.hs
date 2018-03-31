{-# LANGUAGE Arrows #-}
module OSM.XML (parseXMLFile)
where

import Data.Int (Int64)
import Text.XML.HXT.Core
import qualified OSM
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

nothingIfEmpty :: String -> Maybe String
nothingIfEmpty x = if x == "" then Nothing else Just x

numericAttr :: (Read a) => String -> Maybe a
numericAttr x = case reads x of
    [(v,"")] -> Just v
    _        -> Nothing

tagElementToKeyValue :: ArrowXml t => t XmlTree (OSM.TagKey, T.Text)
tagElementToKeyValue = proc el -> do
  key <- getAttrValue "k" -< el
  value <- getAttrValue "v" -< el
  returnA -< (OSM.TagKey (T.pack key), T.pack value)

topLevelTag :: ArrowXml t => String -> t XmlTree XmlTree
topLevelTag tag = getChildren >>> hasName "osm" /> hasName tag

getOSMTags :: ArrowXml t => t XmlTree OSM.Tags
getOSMTags = listA (getChildren >>> hasName "tag" >>> tagElementToKeyValue)
  >>> arr Map.fromList

getOSMID :: ArrowXml a => a XmlTree Int64
getOSMID = getAttrValue "id" >>> arr read

getOSMNode :: ArrowXml t => t XmlTree (OSM.Node ())
getOSMNode = topLevelTag "node" >>>
  proc x -> do
    nodeId <- getOSMID -< x
    tags <- getOSMTags -< x

    latS <- getAttrValue "lat" -< x
    lonS <- getAttrValue "lon" -< x
    let lat = read latS
    let lon = read lonS

    returnA -< OSM.node (OSM.NodeID nodeId) tags
      (OSM.Coordinates { OSM.latitude = lat, OSM.longitude = lon })

getOSMWayNodes :: ArrowXml t => t XmlTree [OSM.NodeID]
getOSMWayNodes = listA $ getChildren >>> hasName "nd" >>> getAttrValue "ref" >>> arr read >>> arr OSM.NodeID

getOSMWay :: ArrowXml t => t XmlTree (OSM.Way ())
getOSMWay = topLevelTag "way" >>>
  proc x -> do
    wayId <- getOSMID -< x
    tags <- getOSMTags -< x
    nodes <- getOSMWayNodes -< x

    returnA -< OSM.way (OSM.WayID wayId) tags nodes

elementIdByType :: String -> Int64 -> OSM.ElementID
elementIdByType "node" i = OSM.ElementNodeID (OSM.NodeID i)
elementIdByType "way" i =  OSM.ElementWayID (OSM.WayID i)
elementIdByType "relation" i = OSM.ElementRelationID (OSM.RelationID i)
elementIdByType t _ = error $ "Invalid type " ++ t

getOSMRelationMembers :: ArrowXml t => t XmlTree [OSM.RelationMember]
getOSMRelationMembers = listA $ getChildren >>> hasName "member" >>>
  proc x -> do
    elIdS <- getAttrValue "ref" -< x
    typeS <- getAttrValue "type" -< x
    role <- getAttrValue "role" -< x
    let elId = read elIdS
    let elementId = elementIdByType typeS elId
    returnA -< OSM.RelationMember elementId (OSM.RelationRole (T.pack role))

getOSMRelation :: ArrowXml t => t XmlTree (OSM.Relation ())
getOSMRelation = topLevelTag "relation" >>>
  proc x -> do
    relationId <- getOSMID -< x
    tags <- getOSMTags -< x
    members <- getOSMRelationMembers -< x

    returnA -< OSM.relation (OSM.RelationID relationId) tags members

listToMap :: (Ord i) => [OSM.Element i p v] -> Map.Map i (OSM.Element i p v)
listToMap list = Map.fromList $ map (\o -> (OSM.id o, o)) list

getOSMEverything :: ArrowXml t => t XmlTree (OSM.Dataset ())
getOSMEverything = proc x -> do
  nodes <- listA getOSMNode -< x
  ways <- listA getOSMWay -< x
  relations <- listA getOSMRelation -< x
  let nodesMap = listToMap nodes
  let waysMap = listToMap ways
  let relationsMap = listToMap relations
  returnA -< OSM.Dataset nodesMap waysMap relationsMap

parseXMLFile :: String -> IO (OSM.Dataset ())
parseXMLFile filename = do
  results <- runX (readDocument [] filename >>> getOSMEverything)
  return (case results of [result] -> result
                          _ -> error "No result")
