{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
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

getOSMNode :: ArrowXml t => t XmlTree OSM.Node
getOSMNode = deep (isElem >>> hasName "node") >>>
  proc x -> do
    idS <- getAttrValue "id" -< x
    user <- getAttrValue "user" -< x
    uidS <- getAttrValue "uid" -< x
    versionS <- getAttrValue "version" -< x
    changesetS <- getAttrValue "changeset" -< x
    timestampS <- getAttrValue "timestamp" -< x
    latS <- getAttrValue "lat" -< x
    lonS <- getAttrValue "lon" -< x
    let nodeId = read idS
    let uid = numericAttr uidS
    let version = numericAttr versionS
    let changeset = numericAttr changesetS
    let timestamp = numericAttr timestampS
    let visible = Nothing
    let versionInfo = OSM.VersionInfo { OSM.user=nothingIfEmpty user
                                      , OSM.uid=uid
                                      , OSM.version=version
                                      , OSM.changeset=changeset
                                      , OSM.timestamp=timestamp
                                      , OSM.visible=visible
                                      }
    let lat = read latS
    let lon = read lonS
    returnA -< OSM.Node (OSM.NodeID nodeId) (OSM.Tags Map.empty) (OSM.Coordinates (OSM.Latitude lat) (OSM.Longitude lon)) versionInfo


main :: IO ()
main = do
  nodes <- runX (readDocument [] "test.xml" >>> getOSMNode)
  print nodes
