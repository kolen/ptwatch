{-# LANGUAGE Arrows #-}
module OSM.Parse (parseXMLFile)
where

import Text.XML.HXT.Core
import qualified OSM
import qualified Data.Map.Strict as Map

nothingIfEmpty :: String -> Maybe String
nothingIfEmpty x = if x == "" then Nothing else Just x

numericAttr :: (Read a) => String -> Maybe a
numericAttr x = case reads x of
    [(v,"")] -> Just v
    _        -> Nothing

tagElementToKeyValue :: ArrowXml t => t XmlTree (OSM.TagKey, String)
tagElementToKeyValue = proc el -> do
  key <- getAttrValue "k" -< el
  value <- getAttrValue "v" -< el
  returnA -< (OSM.TagKey key, value)


topLevelTag :: ArrowXml t => String -> t XmlTree XmlTree
topLevelTag tag = getChildren >>> hasName "osm" /> hasName tag


getOSMTags :: ArrowXml t => t XmlTree OSM.Tags
getOSMTags = listA (getChildren >>> hasName "tag" >>> tagElementToKeyValue)
  >>> arr Map.fromList


getOSMVersionInfo :: ArrowXml a => a XmlTree OSM.VersionInfo
getOSMVersionInfo = proc n -> do
  user <- getAttrValue "user" -< n
  uidS <- getAttrValue "uid" -< n
  versionS <- getAttrValue "version" -< n
  changesetS <- getAttrValue "changeset" -< n
  timestampS <- getAttrValue "timestamp" -< n
  let uid = numericAttr uidS
  let version = numericAttr versionS
  let changeset = numericAttr changesetS
  let timestamp = numericAttr timestampS
  let visible = Nothing
  returnA -< OSM.VersionInfo { OSM.user=nothingIfEmpty user
                             , OSM.uid=uid
                             , OSM.version=version
                             , OSM.changeset=changeset
                             , OSM.timestamp=timestamp
                             , OSM.visible=visible
                             }

getOSMID :: ArrowXml a => a XmlTree Integer
getOSMID = getAttrValue "id" >>> arr read

getOSMNode :: ArrowXml t => t XmlTree OSM.Node
getOSMNode = topLevelTag "node" >>>
  proc x -> do
    nodeId <- getOSMID -< x
    tags <- getOSMTags -< x
    versionInfo <- getOSMVersionInfo -< x

    latS <- getAttrValue "lat" -< x
    lonS <- getAttrValue "lon" -< x
    let lat = read latS
    let lon = read lonS

    returnA -< OSM.Node (OSM.NodeID nodeId) tags (OSM.Coordinates (OSM.Latitude lat) (OSM.Longitude lon)) versionInfo

getOSMWayNodes :: ArrowXml t => t XmlTree [OSM.NodeID]
getOSMWayNodes = listA $ getChildren >>> hasName "nd" >>> getAttrValue "ref" >>> arr read >>> arr OSM.NodeID

getOSMWay :: ArrowXml t => t XmlTree OSM.Way
getOSMWay = topLevelTag "way" >>>
  proc x -> do
    wayId <- getOSMID -< x
    tags <- getOSMTags -< x
    versionInfo <- getOSMVersionInfo -< x

    nodes <- getOSMWayNodes -< x

    returnA -< OSM.Way (OSM.WayID wayId) tags nodes versionInfo

elementIdByType :: String -> Integer -> OSM.ElementID
elementIdByType "node" i = OSM.ElementNodeID (OSM.NodeID i)
elementIdByType "way" i =  OSM.ElementWayID (OSM.WayID i)
elementIdByType "relation" i = OSM.ElementRelationID (OSM.RelationID i)

getOSMRelationMembers :: ArrowXml t => t XmlTree [OSM.RelationMember]
getOSMRelationMembers = listA $ getChildren >>> hasName "member" >>>
  proc x -> do
    elIdS <- getAttrValue "ref" -< x
    typeS <- getAttrValue "type" -< x
    role <- getAttrValue "role" -< x
    let elId = read elIdS
    let elementId = elementIdByType typeS elId
    returnA -< OSM.RelationMember elementId (OSM.RelationRole role)

getOSMRelation :: ArrowXml t => t XmlTree OSM.Relation
getOSMRelation = topLevelTag "relation" >>>
  proc x -> do
    relationId <- getOSMID -< x
    tags <- getOSMTags -< x
    versionInfo <- getOSMVersionInfo -< x

    members <- getOSMRelationMembers -< x

    returnA -< OSM.Relation (OSM.RelationID relationId) tags members versionInfo


listToMap :: (OSM.Element e i) => [e] -> Map.Map i e
listToMap =  Map.fromList . map elementToPair
  where elementToPair el = (OSM.getId el, el)

getOSMEverything :: ArrowXml t => t XmlTree OSM.Dataset
getOSMEverything = proc x -> do
  nodes <- listA getOSMNode -< x
  ways <- listA getOSMWay -< x
  relations <- listA getOSMRelation -< x
  let nodesMap = listToMap nodes
  let waysMap = listToMap ways
  let relationsMap = listToMap relations
  returnA -< OSM.Dataset nodesMap waysMap relationsMap

parseXMLFile :: String -> IO OSM.Dataset
parseXMLFile filename = do
  results <- runX (readDocument [] filename >>> getOSMEverything)
  return (case results of [result] -> result)


main :: IO ()
main = do
  nodes <- runX (readDocument [] "test.xml" >>> getOSMNode)
  print nodes
