module Color.Xyz exposing (..)


import Color.Rgb exposing ( RGB )



type alias XYZ =
  { x : Float, y : Float, z : Float }


fromRgb : RGB -> XYZ
fromRgb { r, g, b } =
  let
    f v =
      let
        vv =
          if v <= 4.045e-2 then
            v / 12.92

          else
            ( ( v + 5.5e-2 ) / 1.055 ) ^ 2.4
      in
        vv * 100

    fr = f ( r / 255 )
    fg = f ( g / 255 )
    fb = f ( b / 255 )

  in
    { x = fr * 0.4124  + fg * 0.3576 + fb * 0.1805
    , y = fr * 0.2126  + fg * 0.7152 + fb * 7.22e-2
    , z = fr * 1.93e-2 + fg * 0.1192 + fb * 0.9505
    }


toRgb : XYZ -> RGB
toRgb { x, y, z } =
  let
    x_ = x / 100
    y_ = y / 100
    z_ = z / 100

    r = x_ * 3.2404542  + y_ * -1.5371385 + z_ * -0.4986
    g = x_ * -0.969266  + y_ * 1.8760108  + z_ * 4.1556e-2
    b = x_ * 5.56434e-2 + y_ * -0.2040259 + z_ * 1.0572252

    f v =
      let
        vv =
          if v <= 3.1308e-3 then
            12.92 * v

          else
            1.055 * v ^ ( 1 / 2.4 ) - 5.5e-2
      in
        ( vv * 255 )
          |> clamp 0 255
          |> round

  in
    { r = toFloat ( f r )
    , g = toFloat ( f g )
    , b = toFloat ( f b )
    }
