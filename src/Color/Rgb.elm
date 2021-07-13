module Color.Rgb exposing (..)

import Hex
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

toCssString : RGB -> String
toCssString { r, g, b }
  = "rgb("
  ++ String.fromFloat ( r * 100 )
  ++ "%, "
  ++ String.fromFloat ( g * 100 )
  ++ "%, "
  ++ String.fromFloat ( b * 100 )
  ++ "%)"


toCssString255 : RGB -> String
toCssString255 { r, g, b }
  = "rgb("
  ++ String.fromInt ( scaleTo255 r ) ++ ", "
  ++ String.fromInt ( scaleTo255 g ) ++ ", "
  ++ String.fromInt ( scaleTo255 b )
  ++ ")"


toHex : RGB -> String
toHex { r, g, b }
  = "#"
  ++ Hex.toString ( scaleTo255 r )
  ++ Hex.toString ( scaleTo255 g )
  ++ Hex.toString ( scaleTo255 b )


scaleTo255 : Float -> Int
scaleTo255 color =
  color
    |> (*) 255
    |> truncate
