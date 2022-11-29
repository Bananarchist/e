module Helpers exposing (..)

duple : a -> (a, a)
duple a = (a, a)

flip : (a -> b -> c) -> b -> a -> c
flip fn p2 p1 = fn p1 p2

tupleMap : (a -> b) -> (a, a) -> (b, b)
tupleMap fn =
  Tuple.mapBoth fn fn
