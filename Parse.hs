{-# LANGUAGE Arrows #-}
module OSM.Parse where

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
    latS <- getAttrValue "lat" -< x
    lonS <- getAttrValue "lon" -< x
    let lat = read latS
    let lon = read lonS

    nodeId <- getOSMID -< x
    tags <- getOSMTags -< x
    versionInfo <- getOSMVersionInfo -< x
    returnA -< OSM.Node (OSM.NodeID nodeId) tags (OSM.Coordinates (OSM.Latitude lat) (OSM.Longitude lon)) versionInfo


-- getOSMWay :: ArrowXml t => t XmlTree OSM.Way
-- getOSMWay = dee


main :: IO ()
main = do
  nodes <- runX (readDocument [] "test.xml" >>> getOSMNode)
  print nodes
