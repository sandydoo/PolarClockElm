module Color.Interpolate exposing (..)

import Array
import Cons exposing (Cons(..))


constant : a -> t -> a
constant a _ =
    a


linear : Float -> Float -> Float -> Float
linear a d t =
    a + d * t


float : Float -> Float -> Float -> Float
float start end time =
    linear start (end - start) time


hue : Float -> Float -> Float -> Float
hue a b t =
    let
        ha =
            normalizeHue a

        hb =
            normalizeHue b

        -- Calculate the shortest distance between hues
        d =
            if abs (hb - ha) <= 180 then
                hb - ha

            else if hb > ha then
                (hb - 360) - ha

            else
                (hb + 360) - ha
    in
    normalizeHue (linear a d t)


normalizeHue : Float -> Float
normalizeHue h =
    if h < 0 then
        normalizeHue (h + 360)

    else if h > 360 then
        normalizeHue (h - 360)

    else
        h


scaleProgress : Int -> Float -> ( Int, Int, Float )
scaleProgress max time =
    let
        position =
            time * toFloat (max - 1)

        bottom =
            floor position

        ( from, to ) =
            if bottom == max then
                ( bottom - 1, bottom )

            else
                ( bottom, bottom + 1 )

        intermediateProgress =
            position - toFloat from
    in
    ( from, to, intermediateProgress )


takeTwoOr : a -> List a -> ( a, a )
takeTwoOr fallback list =
    case list of
        [] ->
            ( fallback, fallback )

        [ a ] ->
            ( a, a )

        a :: b :: _ ->
            ( a, b )


manyVia : (color -> color -> Float -> color) -> Cons color -> (Float -> color)
manyVia interpolate (Cons baseColor colorsL) =
    let
        colors =
            Array.fromList (baseColor :: colorsL)

        count =
            Array.length colors

        toCurrentProgress =
            scaleProgress count

        listOfHelper : Float -> color
        listOfHelper totalProgress =
            let
                ( from, to, progress ) =
                    toCurrentProgress totalProgress

                ( color1, color2 ) =
                    takeTwoOr baseColor
                        << Array.toList
                    <|
                        Array.slice from (to + 1) colors
            in
            interpolate color1 color2 progress
    in
    listOfHelper
