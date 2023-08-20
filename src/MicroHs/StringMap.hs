-- Copyright 2023 Lennart Augustsson
-- See LICENSE file for full license.
module MicroHs.StringMap(
  Map,
  size,
  empty, insert, lookup,
  fromList, fromListWith,
  toList, elems
  ) where
import Prelude --Xhiding(lookup)
--Ximport Compat

{-
import qualified Data.Map as M
import qualified GHC.Maybe

type Map v = M.Map String v

insert = M.insert

fromListWith = M.fromListWith

fromList = M.fromList

--union = M.union

lookup k m =
  case M.lookup k m of
    GHC.Maybe.Nothing -> Nothing
    GHC.Maybe.Just v -> Just v

empty = M.empty

elems = M.elems
-}

-- This is a pretty bad implementation.
data Map v = Map [(String, v)]
  --Xderiving(Show)

insert k v amap =
  case amap of
    Map kvs -> Map ((k, v):kvs)

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

fromList = Map

{-
union akvs1 akvs2 =
  case akvs1 of
    Map kvs1 ->
      case akvs2 of
        Map kvs2 -> Map (kvs1 ++ kvs2)
-}

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

empty = Map []

elems m =
  case m of
    Map kvs -> map snd kvs

size m =
  case m of
    Map kvs -> length kvs

toList (Map kvs) = kvs

{-
import qualified Data.Map as M

type Map v = M.Map String v

insert = M.insertBy leString
fromListWith = M.fromListByWith leString
fromList = M.fromListBy leString
--union = M.unionBy leString
lookup = M.lookupBy leString
empty = M.empty
elems = M.elems
toList = M.toList
-}

-------

insert :: forall v . String -> v -> Map v -> Map v
fromListWith :: forall v . (v -> v -> v) -> [(String, v)] -> Map v
fromList :: forall v . [(String, v)] -> Map v
--union :: forall v . Map v -> Map v -> Map v
lookup :: forall v . String -> Map v -> Maybe v
empty :: forall v . Map v
elems :: forall v . Map v -> [v]
size :: forall v . Map v -> Int
toList :: forall v . Map v -> [(String, v)]
