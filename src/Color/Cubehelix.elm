module Color.Cubehelix exposing (..)

import Color.Interpolate as Interpolate
import Color.Rgb exposing (RGB)



-- A “Cubehelix” colour space, based on the work of David Green, Mike Bostock,
-- and Jason Davies.
--
-- See https://sandydoo.github.io/CubehelixExplained/ for a full explanation.
-- Constants


a =
    -0.14861


b =
    1.78277


c =
    -0.29227


d =
    -0.90649


e =
    1.97294


ed =
    e * d


eb =
    e * b


bcDa =
    b * c - d * a


type Cubehelix
    = Cubehelix
        { h : Float
        , s : Float
        , l : Float
        }


fromRgb : RGB -> Cubehelix
fromRgb rgb =
    let
        red =
            rgb.r / 255

        green =
            rgb.g / 255

        blue =
            rgb.b / 255

        l =
            (bcDa * blue + ed * red - eb * green) / (bcDa + ed - eb)

        x =
            blue - l

        y =
            (e * (green - l) - c * (blue - l)) / d

        s =
            sqrt (x ^ 2 + y ^ 2) / (e * l * (1 - l))

        h =
            atan2 y x * 180 / pi - 120
    in
    Cubehelix
        { h =
            if h < 0 then
                h + 360

            else
                h
        , s = s
        , l = l
        }



-- https://web.archive.org/web/20190814105952/http://astron-soc.in/bulletin/11June/289392011.pdf


toRgb : Cubehelix -> RGB
toRgb (Cubehelix { h, s, l }) =
    let
        hue =
            (h + 120) * pi / 180

        alpha =
            s * l * (1 - l)

        cosH =
            cos hue

        sinH =
            sin hue
    in
    { r = 255 * (l + alpha * (a * cosH + b * sinH))
    , g = 255 * (l + alpha * (c * cosH + d * sinH))
    , b = 255 * (l + alpha * (e * cosH))
    }



-- Interpolate


interpolate : Cubehelix -> Cubehelix -> Float -> Cubehelix
interpolate (Cubehelix col1) (Cubehelix col2) time =
    Cubehelix
        { h = Interpolate.hue col1.h col2.h time
        , s = Interpolate.float col1.s col2.s time
        , l = Interpolate.float col1.l col2.l time
        }
