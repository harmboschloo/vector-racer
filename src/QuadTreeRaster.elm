module QuadTreeRaster exposing
    ( Raster
    , get
    , getSize
    , init
    , set
    , withOutOfBoundsValue
    )

import QuadTreeRaster.Internal as Internal exposing (Model, Size)


type Raster a
    = Raster (Model a)


init : Size -> a -> Raster a
init size initialValue =
    Raster (Internal.init size initialValue)


withOutOfBoundsValue : a -> Raster a -> Raster a
withOutOfBoundsValue outOfBoundsValue (Raster model) =
    Raster (Internal.withOutOfBoundsValue outOfBoundsValue model)


getSize : Raster a -> Size
getSize (Raster model) =
    Internal.getSize model


set : Int -> Int -> a -> Raster a -> Raster a
set x y value (Raster model) =
    Raster (Internal.set x y value model)


get : Int -> Int -> Raster a -> a
get x y (Raster model) =
    Internal.get x y model
