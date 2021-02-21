module Color.Rgb exposing (..)


import Color.Interpolate as Interpolate



type alias RGB =
  { r : Float, g : Float, b : Float }



-- Interpolate


interpolate : RGB -> RGB ->  Float -> RGB
interpolate rgb1 rgb2 time =
  { r = Interpolate.float rgb1.r rgb2.r time
  , g = Interpolate.float rgb1.g rgb2.g time
  , b = Interpolate.float rgb1.b rgb2.b time
  }



-- Serialize


toString { r, g, b }
  = "rgb("
  ++ String.fromFloat r ++ ", "
  ++ String.fromFloat g ++ ", "
  ++ String.fromFloat b
  ++ ")"
