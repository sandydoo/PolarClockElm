module Color.Interpolate exposing (..)



constant : a -> t -> a
constant a _ = a


linear : Float -> Float -> Float -> Float
linear a d t =
  a + d * t


float : Float -> Float -> Float -> Float
float start end time =
  linear start (end - start) time


hue : Float -> Float -> Float -> Float
hue a b =
  let
    d =
      if b > a && b - a > 180 then
        b - a + 360
      else if b < a && a - b > 180 then
        b + 360 - a
      else
        b - a
  in
  linear a d
