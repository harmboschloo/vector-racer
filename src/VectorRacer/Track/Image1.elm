module VectorRacer.Track.Image1 exposing (colors, fromSurfaces)

import QuadTreeRaster
import VectorRacer.Color exposing (Color)
import VectorRacer.Track as Track
import VectorRacer.Track.ImageHelpers as ImageHelpers


colors :
    { road : Color
    , checkpoint1 : Color
    , checkpoint1Inner : Color
    , checkpoint2 : Color
    , checkpoint2Inner : Color
    , checkpoint3 : Color
    , checkpoint3Inner : Color
    , checkpoint4 : Color
    , checkpoint4Inner : Color
    , checkpoint5 : Color
    , checkpoint5Inner : Color
    , curb : Color
    , gravel : Color
    , gravelInner : Color
    , wall : Color
    , wallInner : Color
    }
colors =
    { road = { r = 0xFF, g = 0xFF, b = 0xFF, a = 0xFF }
    , checkpoint1 = { r = 0x68, g = 0xFF, b = 0x68, a = 0xFF }
    , checkpoint1Inner = { r = 0xFF, g = 0xFF, b = 0xFF, a = 0xFF }
    , checkpoint2 = { r = 0xFF, g = 0x68, b = 0x68, a = 0xFF }
    , checkpoint2Inner = { r = 0xFF, g = 0xFF, b = 0xFF, a = 0xFF }
    , checkpoint3 = { r = 0xFF, g = 0x68, b = 0xFF, a = 0xFF }
    , checkpoint3Inner = { r = 0xFF, g = 0xFF, b = 0xFF, a = 0xFF }
    , checkpoint4 = { r = 0xFF, g = 0xB3, b = 0x68, a = 0xFF }
    , checkpoint4Inner = { r = 0xFF, g = 0xFF, b = 0xFF, a = 0xFF }
    , checkpoint5 = { r = 0x00, g = 0x00, b = 0x00, a = 0xFF }
    , checkpoint5Inner = { r = 0xFF, g = 0xFF, b = 0xFF, a = 0xFF }
    , curb = { r = 0xC3, g = 0xC3, b = 0xC3, a = 0xFF }
    , gravel = { r = 0x3C, g = 0x78, b = 0xF0, a = 0xFF }
    , gravelInner = { r = 0xFF, g = 0xFF, b = 0xFF, a = 0xFF }
    , wall = { r = 0x3C, g = 0x78, b = 0xF0, a = 0xFF }
    , wallInner = { r = 0xFF, g = 0xFF, b = 0xFF, a = 0xFF }
    }


fromSurfaces : QuadTreeRaster.Raster Track.Surface -> QuadTreeRaster.Raster Color
fromSurfaces surfaces =
    let
        emptyImage =
            QuadTreeRaster.init (QuadTreeRaster.getSize surfaces) colors.wall

        randomInt =
            0

        ( _, image, _ ) =
            QuadTreeRaster.foldl surfaceToColor ( surfaces, emptyImage, randomInt ) surfaces
    in
    image


surfaceToColor :
    ( Int, Int )
    -> Track.Surface
    -> ( QuadTreeRaster.Raster Track.Surface, QuadTreeRaster.Raster Color, Int )
    -> ( QuadTreeRaster.Raster Track.Surface, QuadTreeRaster.Raster Color, Int )
surfaceToColor point surface ( surfaces, image, randomInt ) =
    let
        ( color, newRandomInt ) =
            getColor point surface surfaces randomInt
    in
    ( surfaces, QuadTreeRaster.set point color image, newRandomInt )


getColor : ( Int, Int ) -> Track.Surface -> QuadTreeRaster.Raster Track.Surface -> Int -> ( Color, Int )
getColor point surface surfaces randomInt =
    case surface of
        Track.Road ->
            ( colors.road, randomInt )

        Track.Checkpoint checkpoint ->
            ( getCheckpointColor point checkpoint surfaces, randomInt )

        Track.Curb ->
            ( colors.curb, randomInt )

        Track.Gravel ->
            getGravelColor point randomInt surfaces

        Track.Wall ->
            ( getWallColor point surfaces, randomInt )


getCheckpointColor : ( Int, Int ) -> Track.Checkpoint -> QuadTreeRaster.Raster Track.Surface -> Color
getCheckpointColor ( x, y ) checkpoint surfaces =
    case checkpoint of
        Track.Checkpoint5 ->
            getFinishColor ( x, y ) surfaces

        _ ->
            let
                ( color, colorInner ) =
                    getCheckpointColors checkpoint
            in
            if
                ImageHelpers.isOnLine (x + y) 6
                    || ImageHelpers.isOnLine (x - y) 6
                    || (ImageHelpers.getAllAround ( x, y ) 3 surfaces
                            |> List.all ((==) (Just (Track.Checkpoint checkpoint)))
                            |> not
                       )
            then
                color

            else
                colorInner


getCheckpointColors : Track.Checkpoint -> ( Color, Color )
getCheckpointColors checkpoint =
    case checkpoint of
        Track.Checkpoint1 ->
            ( colors.checkpoint1, colors.checkpoint1Inner )

        Track.Checkpoint2 ->
            ( colors.checkpoint2, colors.checkpoint2Inner )

        Track.Checkpoint3 ->
            ( colors.checkpoint3, colors.checkpoint3Inner )

        Track.Checkpoint4 ->
            ( colors.checkpoint4, colors.checkpoint4Inner )

        Track.Checkpoint5 ->
            ( colors.checkpoint5, colors.checkpoint5Inner )


getFinishColor : ( Int, Int ) -> QuadTreeRaster.Raster Track.Surface -> Color
getFinishColor ( x, y ) surfaces =
    let
        xOnLine =
            ImageHelpers.isOnLine x 6

        yOnLine =
            ImageHelpers.isOnLine y 6
    in
    if
        (xOnLine && yOnLine)
            || not (xOnLine || yOnLine)
            || (ImageHelpers.getAllAround ( x, y ) 3 surfaces
                    |> List.all ((==) (Just (Track.Checkpoint Track.Checkpoint5)))
                    |> not
               )
    then
        colors.checkpoint5

    else
        colors.checkpoint5Inner


getGravelColor : ( Int, Int ) -> Int -> QuadTreeRaster.Raster Track.Surface -> ( Color, Int )
getGravelColor ( x, y ) randomInt surfaces =
    let
        nextRandomInt =
            ImageHelpers.nextInt randomInt

        color =
            if
                ((nextRandomInt |> modBy 61) == 0)
                    || (ImageHelpers.getAllAround ( x, y ) 1 surfaces
                            |> List.filterMap identity
                            |> List.filter ((/=) Track.Wall)
                            |> List.all ((==) Track.Gravel)
                            |> not
                       )
            then
                colors.gravel

            else
                colors.gravelInner
    in
    ( color, nextRandomInt )


getWallColor : ( Int, Int ) -> QuadTreeRaster.Raster Track.Surface -> Color
getWallColor ( x, y ) surfaces =
    if
        ((x + y |> modBy 20) == 0)
            || (ImageHelpers.getAllAround ( x, y ) 1 surfaces
                    |> List.filterMap identity
                    |> List.all ((==) Track.Wall)
                    |> not
               )
    then
        colors.wall

    else
        colors.wallInner
