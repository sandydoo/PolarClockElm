module Color exposing (..)

import Color.Rgb as Rgb
import Color.DisplayP3 as DisplayP3


type Color
  = None
  | RGB Rgb.RGB
  | DisplayP3 Rgb.RGB


rgb : Float -> Float -> Float -> Color
rgb r g b = RGB { r = r, g = g, b = b }



toHexString : Color -> String
toHexString colorspace =
  case colorspace of
    RGB color -> Rgb.toHex color
    _         -> ""


toCssString : Color -> String
toCssString colorspace =
  case colorspace of
    None ->
      "transparent"

    RGB color ->
      Rgb.toCssString color

    DisplayP3 color ->
      DisplayP3.toCssString color


none : Color
none = None

blue : Color
blue = rgb 0 0 1
