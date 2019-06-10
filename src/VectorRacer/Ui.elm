module VectorRacer.Ui exposing
    ( Size
    , grid
    , gridWithTransform
    , trackBorder
    , trackImage
    )

import Quantity.Vector2 as Vector2 exposing (Vector2)
import Svg exposing (Svg)
import Svg.Attributes
import VectorRacer.Grid as Grid exposing (Grid)
import VectorRacer.Pixels as Pixels exposing (Pixels)


type alias Size =
    Vector2 Int Pixels


grid : Grid -> Svg msg
grid =
    gridWith Nothing


gridWithTransform : String -> Grid -> Svg msg
gridWithTransform transform =
    gridWith (Just transform)


gridWith : Maybe String -> Grid -> Svg msg
gridWith maybeTransform gridModel =
    let
        ( anchorX, anchorY ) =
            Pixels.inPixels (Grid.getAnchorPoint gridModel)

        ( spacingX, spacingY ) =
            Pixels.inPixels (Grid.getSpacing gridModel)

        ( farPointX, farPointY ) =
            Grid.getSpacing gridModel
                |> Vector2.toFloatVector
                |> Vector2.minus (Pixels.pixels ( 0.5, 0.5 ))
                |> Pixels.inPixels
                |> Tuple.mapBoth String.fromFloat String.fromFloat
    in
    Svg.g []
        [ Svg.defs []
            [ Svg.pattern
                ([ Svg.Attributes.id "vectorRacerUiGrid"
                 , Svg.Attributes.x (String.fromInt anchorX)
                 , Svg.Attributes.y (String.fromInt anchorY)
                 , Svg.Attributes.width (String.fromInt spacingX)
                 , Svg.Attributes.height (String.fromInt spacingY)
                 , Svg.Attributes.patternUnits "userSpaceOnUse"
                 ]
                    |> withGridTransform maybeTransform
                )
                [ Svg.polyline
                    [ Svg.Attributes.points ("0.5 " ++ farPointY ++ ", 0.5 0.5 " ++ farPointX ++ " 0.5")
                    , Svg.Attributes.stroke "#3C78F0"
                    , Svg.Attributes.strokeOpacity "0.2"
                    , Svg.Attributes.fill "none"
                    , Svg.Attributes.strokeWidth "1"
                    , Svg.Attributes.strokeLinecap "square"
                    ]
                    []
                ]
            ]
        , Svg.rect
            [ Svg.Attributes.fill "url(#vectorRacerUiGrid)"
            , Svg.Attributes.width "100%"
            , Svg.Attributes.height "100%"
            ]
            []
        ]


withGridTransform : Maybe String -> List (Svg.Attribute msg) -> List (Svg.Attribute msg)
withGridTransform maybeTransform attributes =
    case maybeTransform of
        Just transform ->
            Svg.Attributes.patternTransform transform :: attributes

        Nothing ->
            attributes


trackImage : String -> Size -> Svg msg
trackImage imageSrc trackSize =
    let
        ( trackWidth, trackHeight ) =
            Pixels.inPixels trackSize
    in
    Svg.image
        [ Svg.Attributes.xlinkHref imageSrc
        , Svg.Attributes.width (String.fromInt trackWidth)
        , Svg.Attributes.height (String.fromInt trackHeight)
        ]
        []


trackBorder : Size -> Svg msg
trackBorder trackSize =
    let
        ( trackWidth, trackHeight ) =
            Pixels.inPixels trackSize
    in
    Svg.rect
        [ Svg.Attributes.x "-1"
        , Svg.Attributes.y "-1"
        , Svg.Attributes.width (String.fromInt (trackWidth + 2))
        , Svg.Attributes.height (String.fromInt (trackHeight + 2))
        , Svg.Attributes.fill "none"
        , Svg.Attributes.stroke "#3C78F0"
        , Svg.Attributes.strokeWidth "2"
        ]
        []
