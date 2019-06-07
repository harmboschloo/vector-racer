module VectorRacer.Track.ImageHelpers exposing
    ( bytesEncoder
    , getAllAround
    , isOnLine
    , nextInt
    )

import Bytes.Encode
import QuadTreeRaster
import VectorRacer.Color as Color exposing (Color)


nextInt : Int -> Int
nextInt value =
    (value * 1713 + 1) |> modBy 65536


isOnLine : Int -> Int -> Bool
isOnLine value thickness =
    let
        v =
            if value < 0 then
                thickness - value

            else
                value
    in
    (v |> modBy (2 * thickness)) < thickness


getAllAround : ( Int, Int ) -> Int -> QuadTreeRaster.Raster a -> List (Maybe a)
getAllAround ( x, y ) range raster =
    QuadTreeRaster.foldlRegion
        { min = ( x - range, y - range )
        , max = ( x + range, y + range )
        }
        (\( px, py ) value result ->
            if ( px, py ) == ( x, y ) then
                result

            else
                let
                    dx =
                        px - x

                    dy =
                        py - y
                in
                if dx * dx + dy * dy <= range * range + 1 then
                    value :: result

                else
                    result
        )
        []
        raster


bytesEncoder : QuadTreeRaster.Raster Color -> Bytes.Encode.Encoder
bytesEncoder image =
    image
        |> QuadTreeRaster.foldr (\_ color sequence -> Color.bytesEncoder color :: sequence) []
        |> Bytes.Encode.sequence
