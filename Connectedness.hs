module Ptwatch.Connectedness
where

import qualified OSM
import qualified Data.Map.Strict as Map

type WaysSeq = [OSM.Way]

data WayWithDirection = WayWithDirection OSM.Way Direction
data WayWithUncertainDirection =
   WayWithUncertainDirection OSM.Way UncertainDirection

data Direction = Forward | Backward
data UncertainDirection = KnownDirection Direction | UnknownDirection
data Oneway = Oneway | ReverseOneway | NotOneway

onewayDirection :: Oneway -> UncertainDirection
onewayDirection Oneway = KnownDirection Forward
onewayDirection ReverseOneway = KnownDirection Backward
onewayDirection NotOneway = UnknownDirection

oneway :: OSM.Way -> Oneway
oneway = ov . Map.lookup (OSM.TagKey "oneway") . OSM.tags
  where
    ov :: Maybe String -> Oneway
    ov (Just x) | x `elem` ["yes", "1", "true"] = Oneway
                | x `elem` ["reversible", "-1", "reverse"] = ReverseOneway
                | otherwise = NotOneway
    ov Nothing = NotOneway

clarify :: UncertainDirection -> UncertainDirection -> UncertainDirection
clarify (KnownDirection d1) _ = KnownDirection d1
clarify UnknownDirection d2 = d2

wayDirectionByItself :: OSM.Way -> UncertainDirection
wayDirectionByItself = onewayDirection . oneway

startPoint :: WayWithDirection -> OSM.NodeID
startPoint (WayWithDirection w Forward) = head $ OSM.nodeIds w
startPoint (WayWithDirection w Backward) = last $ OSM.nodeIds w

endPoint :: WayWithDirection -> OSM.NodeID
endPoint (WayWithDirection w Forward) = last $ OSM.nodeIds w
endPoint (WayWithDirection w Backward) = head $ OSM.nodeIds w

canBePaired :: WayWithDirection -> WayWithDirection -> Bool
canBePaired w1 w2 = endPoint w1 == startPoint w2

wayDirectionByPair :: Maybe WayWithUncertainDirection -> OSM.Way -> Maybe UncertainDirection
wayDirectionByPair = undefined

wayDirection :: Maybe WayWithUncertainDirection -> OSM.Way -> [OSM.Way] -> Maybe UncertainDirection
wayDirection prev way nexts = do
  let itself = wayDirectionByItself way
  pair <- wayDirectionByPair prev way
  next <- nextPair pair way nexts
  return $ itself `clarify` pair `clarify` next
    where
      nextPair :: UncertainDirection -> OSM.Way -> [OSM.Way] -> Maybe UncertainDirection
      nextPair _ _ [] = Just UnknownDirection
      nextPair prevDir w (next1:nexts1) =
        wayDirection (Just (WayWithUncertainDirection w prevDir)) next1 nexts1
