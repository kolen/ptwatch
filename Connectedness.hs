module Ptwatch.Connectedness
where

import qualified OSM
import qualified Data.Map.Strict as Map

type WaysSeq = [OSM.Way]

data WayWithDirection = WayWithDirection OSM.Way Direction

data Direction = Forward | Backward | Unknown
data Oneway = Oneway | ReverseOneway | NotOneway

oneway :: OSM.Way -> Oneway
oneway = ov . Map.lookup (OSM.TagKey "oneway") . OSM.tags
  where
    ov :: Maybe String -> Oneway
    ov (Just x) | x `elem` ["yes", "1", "true"] = Oneway
                | x `elem` ["reversible", "-1", "reverse"] = ReverseOneway
                | otherwise = NotOneway
    ov Nothing = NotOneway
