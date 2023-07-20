-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.StringMap(module MicroHs.StringMap) where
import Prelude
--Ximport Compat

{-
import qualified Data.Map as M
import qualified GHC.Maybe

type Map v = M.Map String v

insert :: String -> v -> Map v -> Map v
insert = M.insert

fromListWith :: (v -> v -> v) -> [(String, v)] -> Map v
fromListWith = M.fromListWith

fromList :: [(String, v)] -> Map v
fromList = M.fromList

union :: Map v -> Map v -> Map v
union = M.union

lookup :: String -> Map v -> Maybe v
lookup k m =
  case M.lookup k m of
    GHC.Maybe.Nothing -> Nothing
    GHC.Maybe.Just v -> Just v

empty :: Map v
empty = M.empty

elems :: Map v -> [v]
elems = M.elems
-}

-- This is a pretty bad implementation.
data Map v = Map [(String, v)]
  --Xderiving(Show)

insert :: String -> v -> Map v -> Map v
insert k v amap =
  case amap of
    Map kvs -> Map ((k, v):kvs)

fromListWith :: (v -> v -> v) -> [(String, v)] -> Map v
fromListWith un =
  let
    ins ikv akvs =
      case akvs of
        [] -> [ikv]
        kv : kvs ->
          case ikv of
            (ik, iv) ->
              case kv of
                (k, v) ->
                  if eqString ik k then
                    (k, un iv v) : kvs
                  else
                    kv : ins ikv kvs
  in
     Map . foldr ins []

fromList :: [(String, v)] -> Map v
fromList = Map

union :: Map v -> Map v -> Map v
union akvs1 akvs2 =
  case akvs1 of
    Map kvs1 ->
      case akvs2 of
        Map kvs2 -> Map (kvs1 ++ kvs2)

lookup :: String -> Map v -> Maybe v
lookup ak am =
  case am of
    Map m ->
      let
        look akvs =
          case akvs of
            [] -> Nothing
            kv : kvs ->
              case kv of
                (k, v) -> if eqString ak k then Just v else look kvs
      in look m

empty :: Map v
empty = Map []

elems :: Map v -> [v]
elems m =
  case m of
    Map kvs -> map snd kvs
